{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Delegate where

import           Control.Monad             hiding (fmap)
-- For making Parameters
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.List                 (find)
import qualified Data.Map                  as Map
import           Data.Maybe                (mapMaybe)
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Ledger                    hiding (singleton)
import           Ledger.Ada                as Ada hiding (divide)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Plutus.Contract           as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup (..), find,
                                            mapMaybe, unless, (<$>))

import           Playground.Contract       (ToSchema)
import           Plutus.Contracts.Currency as Currency

import           Funds
import           Prelude                   (Semigroup (..), (<$>))
import           Token


data Royalty = Royalty
  { rCreator       :: !PubKeyHash,
    rBeneficiaries :: ![(PubKeyHash, Integer)], --  Mapping percentage to PubKeyHash of wallet
    rToken         :: !AssetClass, --  The token to be used for the royalty
    rNFT           :: !CurrencySymbol, --  To identify specific utxo
    rDeadline      :: !Slot --  Time in posix upto which the royalty is to be paid
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''Royalty

-- Delegation means that the creator of the token can delegate the royalty to other wallets

data DelegateDatum = DelegateDatum
  { costPrice :: !Integer --  Cost of Token set by creator for seller
  }
  deriving (Show, Generic, ToJSON, FromJSON)

instance Eq DelegateDatum where
    {-# INLINABLE (==) #-}
    a == b = (costPrice a == costPrice b)


PlutusTx.makeLift ''DelegateDatum
PlutusTx.unstableMakeIsData ''DelegateDatum

data DelegateRedeemer = Update | Retrieve Integer | Take Integer -- Take means MarketPlace wallet buys the token to put it for sale.
  deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''DelegateRedeemer
PlutusTx.makeLift ''DelegateRedeemer

-- Main Validator
mkDelegateValidator :: Royalty -> DelegateDatum -> DelegateRedeemer -> ScriptContext -> Bool
mkDelegateValidator royalty ddt r ctx =
  traceIfFalse "NFT missing from input"  inputHasNFT  &&
  case r of
    Update   -> traceIfFalse "Creator signature missing" (txSignedBy info $ rCreator royalty) &&
                traceIfFalse "NFT missing from output"  outputHasNFT 

    Retrieve nToken -> traceIfFalse "operator signature missing" (txSignedBy info $ rCreator royalty) &&
                       traceIfFalse "NFT missing from output"  (outputHasNFT' nToken)
    Take nToken ->  True --validateTake nToken 

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "Input missing"
      Just i  -> txInInfoResolved i 

    inputHasNFT :: Bool 
    inputHasNFT = assetClassValueOf (txOutValue ownInput) (royaltyAsset royalty) == 1

    ownOutput :: TxOut
    ownOutput  = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one royalty output"

    numOfInToken :: Integer
    numOfInToken = assetClassValueOf (txOutValue ownInput) (rToken royalty)

    outputHasNFT :: Bool
    outputHasNFT = assetClassValueOf (txOutValue ownOutput) (royaltyAsset royalty) == 1

    outputHasNFT' :: Integer -> Bool
    outputHasNFT' nToken =  if numOfInToken == nToken then True else assetClassValueOf (txOutValue ownOutput) (royaltyAsset royalty) == 1


    outputDatum :: Maybe DelegateDatum
    outputDatum = delegateDatum ownOutput (`findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum =  outputDatum == Just ddt

    nftPlusAda :: Integer -> Value 
    nftPlusAda nToken = assetClassValue (royaltyAsset royalty) 1 <> Ada.lovelaceValueOf (nToken * (costPrice ddt))

    validateTake :: Integer -> Bool
    validateTake nToken 
      | numOfInToken == nToken = traceError "Creator did not get both token and cost price" (getsValue (rCreator royalty) $ nftPlusAda nToken)
      -- | otherwise = traceError "Creator not paid" (getsValue (rCreator royalty) $ Ada.lovelaceValueOf (nToken * (costPrice ddt))) &&
      | otherwise = traceError "Creator not paid" (lovelacePaidTo (rCreator royalty) (nToken * (costPrice ddt))) &&
                    traceError "The datum is not valid" validOutputDatum  && 
                    traceError "The NFT is missing from output" outputHasNFT 
                    
                    -- traceError "Creator not paid" (getsValue (rCreator royalty) $ nftPlusAda 0)


    lovelacePaidTo :: PubKeyHash -> Integer -> Bool
    lovelacePaidTo pkh amt =
      let
        pkhValue :: Integer
        pkhValue = lovelaces (valuePaidTo info pkh)
      in
        pkhValue >= amt

    getsValue :: PubKeyHash -> Value -> Bool
    getsValue h v =
      let
        [o] = [ o'
              | o' <- txInfoOutputs info
              , txOutValue o' == v
              ]
      in
        txOutAddress o == pubKeyHashAddress h



data Delegating

instance Scripts.ScriptType Delegating where
  type DatumType Delegating = DelegateDatum
  type RedeemerType Delegating = DelegateRedeemer


{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue



{-# INLINEABLE emptyTokenName #-}
-- For token name of NFT
emptyTokenName :: TokenName
emptyTokenName = TokenName emptyByteString

{-# INLINEABLE royaltyAsset #-}
royaltyAsset :: Royalty -> AssetClass
royaltyAsset royalty = AssetClass (rNFT royalty, emptyTokenName)

{-# INLINEABLE delegateDatum #-}

delegateDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe DelegateDatum
delegateDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromData d


delegateInst :: Royalty -> Scripts.ScriptInstance Delegating
delegateInst royalty =
  Scripts.validator @Delegating
    ($$(PlutusTx.compile [||mkDelegateValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode royalty)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @DelegateDatum @DelegateRedeemer

delegateValidator :: Royalty -> Validator
delegateValidator = Scripts.validatorScript . delegateInst

delegateAddress :: Royalty -> Ledger.Address
delegateAddress = scriptAddress . delegateValidator

data RoyaltyParams = RoyaltyParams
  { rpNumToken      :: !Integer, -- Number of token the creator wants to put in market place
    rpPrice         :: !Integer, -- Price in lovelace
    rpSelfPercent   :: !Integer,
    rpBeneficiaries :: ![(PubKeyHash, Integer)],
    rpToken         :: !AssetClass,
    rpDeadline      :: !Slot
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | [input] : rp (parameters to construct Royalty data instance)
--   [output]: Contract (It creates Royalty instance and tells to log which can later be accessed)
createRoyalty :: RoyaltyParams -> Contract (Last Royalty) BlockchainActions Text ()
createRoyalty rp = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  osc <- mapError (pack . show) (forgeContract pkh [(emptyTokenName, 1)] :: Contract (Last Royalty) BlockchainActions CurrencyError OneShotCurrency)
  let cs = Currency.currencySymbol osc
      royalty =
        Royalty
          { rCreator = pkh,
            rBeneficiaries = rpBeneficiaries rp,
            rToken = rpToken rp,
            rNFT = cs,
            rDeadline = rpDeadline rp
          }

      val = assetClassValue (rpToken rp) (rpNumToken rp) <> assetClassValue (royaltyAsset royalty) 1

      tx = Constraints.mustPayToTheScript (DelegateDatum $ rpPrice rp) val
  tell $ Last $ Just royalty
  ledgerTx <- submitTxConstraints (delegateInst royalty) tx

  awaitTxConfirmed $ txId ledgerTx
  logInfo @String $ show (rpToken rp) ++ " token kept in market place for trade"

-- | It is for searching required UtXo
findRoyalty :: 
  forall w s. HasBlockchainActions s =>
  Royalty ->Contract w s Text (Maybe (TxOutRef, TxOutTx, DelegateDatum))
findRoyalty royalty = do
  utxos <- Map.filter f <$> utxoAt (delegateAddress royalty)
  return $ case Map.toList utxos of
    [(oref, o)] -> do
      dt <- delegateDatum (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
      return (oref, o, dt)
    _ -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (royaltyAsset royalty) == 1

-- | It updates the price of token
updateRoyalty ::
  forall w s. HasBlockchainActions s => 
  Royalty -> Integer -> Contract w s Text ()
updateRoyalty royalty cp' = do
  m <- findRoyalty royalty
  case m of
    Nothing -> do
      throwError "No royalty found for update"
    Just (oref, o, _) -> do
      let
        val = txOutValue (txOutTxOut o)

        lookups =Constraints.unspentOutputs (Map.singleton oref o)        <>
                 Constraints.scriptInstanceLookups (delegateInst royalty) <>
                Constraints.otherScript (delegateValidator royalty)

        tx = Constraints.mustPayToTheScript (DelegateDatum cp') val <>
             Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Update)

      ledgerTx <- submitTxConstraintsWith @Delegating lookups tx
      awaitTxConfirmed $ txId ledgerTx
      logInfo @String $ "updated cost price to " ++ show cp'




type DelegateSchema =
  BlockchainActions .\/
  Endpoint "update"   (Royalty, Integer) .\/
  Endpoint "retrieve" (Royalty, Integer)





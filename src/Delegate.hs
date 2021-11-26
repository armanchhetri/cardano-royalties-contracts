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
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''DelegateDatum
PlutusTx.unstableMakeIsData ''DelegateDatum

data DelegateRedeemer = Update | Retrieve | Take Integer -- Take means MarketPlace wallet buys the token to put it for sale.
  deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''DelegateRedeemer
PlutusTx.makeLift ''DelegateRedeemer

-- Main Validator
mkDelegateValidator :: Royalty -> DelegateDatum -> DelegateRedeemer -> ScriptContext -> Bool
mkDelegateValidator royalty ddt r ctx =
    case r of
      Update   -> traceIfFalse "invalid output datum" validOutputDatum                          &&
                  traceIfFalse "tokens are not returned to script" tokenReturned                &&
                  traceIfFalse "operator signature missing" (txSignedBy info $ rCreator royalty)

      Retrieve -> traceIfFalse "operator signature missing" (txSignedBy info $ rCreator royalty)  

      Take rpNumToken -> traceIfFalse "Creator not paid " creatorPaid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validOutputDatum :: Bool
    validOutputDatum = True  --TODO

    tokenReturned :: Bool
    tokenReturned = True  --TODO

    creatorPaid :: Bool
    creatorPaid = True  --TODO


data Delegating

instance Scripts.ScriptType Delegating where
  type DatumType Delegating = DelegateDatum
  type RedeemerType Delegating = DelegateRedeemer

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
  Endpoint "update" (Royalty, Integer)
  -- Endpoint "test"   ()





endpoints :: Contract () DelegateSchema Text ()
endpoints = (update) >> endpoints
  where
    -- create = endpoint @"create" >>= createRoyalty

    -- test = endpoint @"test" >> myContract4

    update :: Contract () DelegateSchema Text ()
    update = do
      (royalty, cp') <- endpoint @"update"
      updateRoyalty royalty cp'


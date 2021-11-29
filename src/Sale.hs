{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Sale where

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

import           Delegate
import           Funds
import           Prelude                   (Semigroup (..), (<$>))



data Sale = Sale
    {
          sSeller      :: !PubKeyHash   -- Marketplace wallet who wants to sell token
        , sRoyaltyRate :: !Integer      -- Amount in lovelaces the market place is willing to pay as royalty for each token sold
                                        -- which is later to be distributed to the wallet specified by the creator
        , sNFT         :: !CurrencySymbol -- NFT to identify the sale utxo
    }deriving (Eq, Show, Generic, ToJSON, FromJSON)


PlutusTx.makeLift ''Sale


newtype SaleDatum = SaleDatum {salePrice :: Integer}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''SaleDatum
PlutusTx.unstableMakeIsData ''SaleDatum

data SaleRedeemer = Buy Integer | UpdateSale | RetrieveSale Integer
  deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''SaleRedeemer
PlutusTx.makeLift ''SaleRedeemer



{-# INLINABLE royaltyAmount #-}
royaltyAmount :: [(PubKeyHash, Integer)] -> Integer -> [(PubKeyHash, Integer)]
royaltyAmount beneficiaries total = [(pkh, f percent ) | (pkh, percent) <- beneficiaries]
                                where
                                    f :: Integer -> Integer
                                    f  percent = round $ (total * percent) % 100

{-# INLINEABLE saleAsset #-}
saleAsset :: Sale -> AssetClass
saleAsset sale = AssetClass (sNFT sale, emptyTokenName)

{-# INLINEABLE saleDatum #-}

saleDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe SaleDatum
saleDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromData d


-- Main Validator For Sale
-- It has two parameters sale gives further information for the buyer of token.
{-# INLINABLE mkSaleValidator #-}
mkSaleValidator :: Royalty -> Sale -> SaleDatum -> SaleRedeemer -> ScriptContext -> Bool
mkSaleValidator royalty sale SaleDatum{..} r ctx =
  traceIfFalse "NFT missing from input"  inputHasNFT  &&
  case r of
      UpdateSale  -> traceIfFalse "No authorized signature found" authenticateSeller &&
                     traceIfFalse "NFT missing from sale output"  outputHasNFT 
                    

      RetrieveSale nToken  -> traceIfFalse "No authorized signature found" authenticateSeller &&
                              traceIfFalse "NFT missing from output"  (outputHasNFT' nToken)

      Buy nToken -> traceIfFalse "Seller Not paid yet"     (sellerPaid nToken) &&
                      traceIfFalse "Royalty not distributed" (royaltyDistributed nToken)


  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "Input missing"
      Just i  -> txInInfoResolved i 

    inputHasNFT :: Bool 
    inputHasNFT = assetClassValueOf (txOutValue $  ownInput) (saleAsset sale) == 1

    ownOutput :: TxOut
    ownOutput  = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one royalty output"


    -- Exactly one value of NFT
    outputHasNFT :: Bool
    outputHasNFT = assetClassValueOf (txOutValue ownOutput) (saleAsset sale) == 1

    -- number of token from input utxo
    numOfInToken :: Integer
    numOfInToken = available ownInput royalty

    -- condition of nft returned for Retrieve Sale
    outputHasNFT' :: Integer -> Bool
    outputHasNFT' nToken =  numOfInToken == nToken || assetClassValueOf (txOutValue ownOutput) (saleAsset sale) == 1


    authenticateSeller :: Bool
    authenticateSeller = txSignedBy info (sSeller sale)

    sellerGets :: Integer -> Integer
    sellerGets = (*) (salePrice - sRoyaltyRate sale)

    sellerPaid :: Integer -> Bool
    sellerPaid nToken = lovelacePaidTo (sSeller sale) $ sellerGets nToken

    totalRoyalty :: Integer -> [(PubKeyHash, Integer)]
    totalRoyalty nToken= royaltyAmount (rBeneficiaries royalty) (nToken * sRoyaltyRate sale)

    royaltyDistributed :: Integer -> Bool
    royaltyDistributed nToken = royaltyFor $ totalRoyalty nToken
                where
                    royaltyFor :: [(PubKeyHash, Integer)] -> Bool
                    royaltyFor [] = True
                    royaltyFor ((pkh, amt):bs) = lovelacePaidTo pkh amt && royaltyFor bs

    lovelacePaidTo :: PubKeyHash -> Integer -> Bool
    lovelacePaidTo pkh amt =
      let
        pkhValue :: Integer
        pkhValue = lovelaces (valuePaidTo info pkh)
      in
        pkhValue >= amt



data Selling
instance Scripts.ScriptType Selling where
    type instance DatumType Selling = SaleDatum
    type instance RedeemerType Selling = SaleRedeemer

saleInst :: Royalty -> Sale -> Scripts.ScriptInstance Selling
saleInst royalty sale = Scripts.validator @Selling
    ($$(PlutusTx.compile [|| mkSaleValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode royalty
        `PlutusTx.applyCode` PlutusTx.liftCode sale)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @SaleDatum @SaleRedeemer

saleValidator :: Royalty -> Sale -> Validator
saleValidator royalty = Scripts.validatorScript . saleInst royalty

saleAddress :: Royalty -> Sale -> Ledger.Address
saleAddress royalty = scriptAddress . saleValidator royalty


data SaleParams = SaleParams {
      spSellingPrice:: !Integer
    , spRoyaltyRate  :: !Integer
    , spNumToken     :: !Integer -- Number of token to sell
    }



putOnSale ::
  forall s. HasBlockchainActions s =>
  (Royalty, SaleParams) -> Contract (Last Sale) s Text ()
putOnSale (royalty, sp) = do
  m <- findRoyalty royalty

  case m of
    Nothing -> throwError "Royalty not found"

    Just (oref, o, ddt@DelegateDatum{..}) | (available (txOutTxOut o) royalty) >= spNumToken sp -> do

      pkh <- pubKeyHash <$> Contract.ownPubKey
      osc <- mapError (pack . show) (forgeContract pkh [(emptyTokenName, 1)] :: Contract (Last Sale) s CurrencyError OneShotCurrency)
      let
        cs = Currency.currencySymbol osc
        sale = Sale {
              sSeller      = pkh,
              sRoyaltyRate = spRoyaltyRate sp,
              sNFT         = cs
          }
        toReturn = available (txOutTxOut o) royalty - spNumToken sp

        (val1, val3) = if available (txOutTxOut o) royalty == spNumToken sp then
                        (token, ada <> nft)          else
                        (token <> nft , ada)
                        where
                          token = assetClassValue (rToken royalty) toReturn
                          nft = assetClassValue (royaltyAsset royalty) 1
                          ada = Ada.lovelaceValueOf (spNumToken sp * costPrice)

        val2 = assetClassValue (rToken royalty) (spNumToken sp) <> assetClassValue (AssetClass (cs, emptyTokenName)) 1 -- For Sale script


        lookups = Constraints.unspentOutputs ( Map.singleton oref o)   <>
                  Constraints.otherScript (saleValidator royalty sale) <>
                  Constraints.otherScript (delegateValidator royalty) 
                  

        tx      = Constraints.mustPayToPubKey (rCreator royalty) val3 <>
                  Constraints.mustPayToOtherScript (validatorHash $ saleValidator royalty sale)
                      (Datum $ PlutusTx.toData $ SaleDatum $ spSellingPrice sp) val2                          <>
                  Constraints.mustPayToOtherScript (validatorHash $ delegateValidator royalty)
                      (Datum $ PlutusTx.toData ddt) val1 <>
                  Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toData (Take (spNumToken sp)))                  
                  
      tell $ Last $ Just sale
      ledgerTx <- submitTxConstraintsWith @Selling lookups tx
      awaitTxConfirmed $ txId ledgerTx
      logInfo @String $ "Kept token " ++ show (rToken royalty) ++ "for the sale at price " ++ show (spSellingPrice sp)

    _ -> throwError "Number of token parameter not valid"



-- | It is for searching required UtXo
findSale ::
  forall w s. HasBlockchainActions s =>
  (Royalty, Sale) -> Contract w s Text (Maybe (TxOutRef, TxOutTx, SaleDatum))
findSale (royalty, sale) = do
  utxos <- Map.filter f <$> utxoAt (saleAddress royalty sale)
  return $ case Map.toList utxos of
    [(oref, o)] -> do
      dt <- saleDatum (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
      return (oref, o, dt)
    _ -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (saleAsset sale) == 1


-- | It updates the selling price of token
updateSale ::
  forall w s. HasBlockchainActions s =>
  (Royalty, Sale) -> Integer -> Contract w s Text  ()
updateSale (royalty, sale) sp'  = do
  m <- findSale (royalty, sale)
  case m of
    Nothing -> do
      throwError "No sale found for update"
    Just (oref, o, _) -> do
      let
        val = txOutValue (txOutTxOut o)

        lookups = Constraints.unspentOutputs (Map.singleton oref o)         <>
                  Constraints.scriptInstanceLookups (saleInst royalty sale) <>
                  Constraints.otherScript (saleValidator royalty sale)

        tx      = Constraints.mustPayToTheScript (SaleDatum sp') val <>
                  Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData UpdateSale)

      ledgerTx <- submitTxConstraintsWith @Selling lookups tx
      awaitTxConfirmed $ txId ledgerTx
      logInfo @String $ "updated selling price to " ++ show sp'


buy ::
  forall s w. HasBlockchainActions s =>
  (Royalty, Sale) -> Integer -> Contract w s Text ()
buy (royalty, sale) num = do
  m <- findSale (royalty, sale)
  case m of
    Nothing -> do
      throwError "No sale found for buy"
    Just (oref, o, dt@SaleDatum{..}) |  (available (txOutTxOut o) royalty) >= num && (salePrice > sRoyaltyRate sale) -> do
      let
        price        = salePrice * num
        totalRoyalty = (sRoyaltyRate sale) * num
        av = available (txOutTxOut o) royalty

        (valSlr, valScr) = if av == num       then
                          (ada <> nft, token) else
                          (ada, token <> nft)
                          where
                            token = assetClassValue (rToken royalty) (av-num)
                            nft = assetClassValue (saleAsset sale) 1
                            ada = Ada.lovelaceValueOf (price - totalRoyalty)

        beneficiaries = royaltyAmount (rBeneficiaries royalty) totalRoyalty



        lookups = Constraints.unspentOutputs (Map.singleton oref o)         <>
                  Constraints.scriptInstanceLookups (saleInst royalty sale) <>
                  Constraints.otherScript (saleValidator royalty sale)

        tx      = mconcat [Constraints.mustPayToPubKey pkh (Ada.lovelaceValueOf val) | (pkh,val) <- beneficiaries]   <>
                  Constraints.mustPayToPubKey (sSeller sale) valSlr                                                  <>
                  Constraints.mustPayToOtherScript (validatorHash $ saleValidator royalty sale)
                      (Datum $ PlutusTx.toData dt) valScr                                                             <>
                  Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData (Buy num))

      ledgerTx <- submitTxConstraintsWith @Selling lookups tx
      awaitTxConfirmed $ txId ledgerTx
      logInfo @String $ "bought " ++ show num ++ " tokens"

    _ -> throwError "Number of token parameter not valid"  

retrieveSale :: 
  forall s w. HasBlockchainActions s =>
  (Royalty, Sale) -> Integer -> Contract w s Text ()
retrieveSale (royalty, sale) num = do
  m <- findSale (royalty, sale)
  case m of
    Nothing -> do
      throwError "No sale found to retrieve"
    Just (oref, o, dt) |  (available (txOutTxOut o) royalty) >= num -> do
      let
        
        av = available (txOutTxOut o) royalty
        valScr = assetClassValue (rToken royalty) (av-num) <> assetClassValue (saleAsset sale) 1
       

        lookups = Constraints.unspentOutputs (Map.singleton oref o)         <>
                  Constraints.scriptInstanceLookups (saleInst royalty sale) <>
                  Constraints.otherScript (saleValidator royalty sale)

        tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData (RetrieveSale num)) <>
                  if av /= num then 
                    Constraints.mustPayToOtherScript (validatorHash $ saleValidator royalty sale)
                    (Datum $ PlutusTx.toData dt) valScr  else
                    mempty 
        

      ledgerTx <- submitTxConstraintsWith @Selling lookups tx
      awaitTxConfirmed $ txId ledgerTx
      logInfo @String $ "Retrieved " ++ show num ++ " tokens"

    _ -> throwError "Number of token parameter not valid"



type SaleSchema =
  BlockchainActions .\/
  Endpoint "updateSale"   Integer .\/ -- Integer is price
  Endpoint "retrieveSale" Integer .\/ -- Integer is num of token
  Endpoint "buy"          Integer     -- Integer is num of token


dEndpoints :: Royalty -> Contract () DelegateSchema Text ()
dEndpoints royalty = (update `select` retrieve') >> dEndpoints royalty
  where
    update :: Contract () DelegateSchema Text ()
    update = do
      cp' <- endpoint @"update"
      updateRoyalty royalty cp'

    retrieve' :: Contract () DelegateSchema Text ()
    retrieve' = do
      num <- endpoint @"retrieve"
      retrieve royalty num 


sEndpoints :: (Royalty, Sale) -> Contract () SaleSchema Text ()
sEndpoints (royalty, sale)= (updateSale' `select` buy' `select` retrieveSale') >> sEndpoints (royalty, sale)
  where
    updateSale' :: Contract () SaleSchema Text ()
    updateSale' = do
      sp' <- endpoint @"updateSale"
      updateSale (royalty, sale) sp'

    buy' :: Contract () SaleSchema Text ()
    buy' = do
      num <- endpoint @"buy"
      buy (royalty, sale) num


    retrieveSale' :: Contract () SaleSchema Text ()
    retrieveSale' = do
      num <- endpoint @"retrieveSale"
      retrieveSale (royalty, sale) num







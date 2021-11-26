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
        , sNFT       :: !CurrencySymbol -- NFT to identify the sale utxo
    }deriving (Eq, Show, Generic, ToJSON, FromJSON)


PlutusTx.makeLift ''Sale


newtype SaleDatum = SaleDatum {salePrice :: Integer}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''SaleDatum
PlutusTx.unstableMakeIsData ''SaleDatum

data SaleRedeemer = Buy Integer | UpdatePrice | RetrieveSale
  deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''SaleRedeemer
PlutusTx.makeLift ''SaleRedeemer


{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue



{-# INLINABLE royaltyAmount #-}
royaltyAmount :: [(PubKeyHash, Integer)] -> Integer -> [(PubKeyHash, Integer)]
royaltyAmount beneficiaries total = [(pkh, f percent ) | (pkh, percent) <- beneficiaries]
                                where
                                    f :: Integer -> Integer
                                    f  percent = round $ (total * percent) % 100


-- Main Validator For Sale
-- It has two parameters sale gives further information for the buyer of token.
{-# INLINABLE mkSaleValidator #-}
mkSaleValidator :: Royalty -> Sale -> SaleDatum -> SaleRedeemer -> ScriptContext -> Bool
mkSaleValidator royalty sale SaleDatum{..} r ctx =
  case r of
      UpdatePrice  -> traceIfFalse "invalid output datum" validOutputDatum                     &&
                      traceIfFalse "No authorized signature found" authenticateSeller          &&
                      traceIfFalse "Token did not return to script address" outputHasToken

      RetrieveSale -> traceIfFalse "No authorized signature found" authenticateSeller

      Buy numToken -> traceIfFalse "Seller Not paid yet"     (sellerPaid numToken) &&
                      traceIfFalse "Royalty not distributed" (royaltyDistributed numToken)


  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validOutputDatum :: Bool
    validOutputDatum = True  -- TODO

    outputHasToken :: Bool
    outputHasToken = True  -- TODO

    authenticateSeller :: Bool
    authenticateSeller = txSignedBy info (sSeller sale)



    sellerGets :: Integer -> Integer
    sellerGets = (*) (salePrice - sRoyaltyRate sale)

    sellerPaid :: Integer -> Bool
    sellerPaid numToken = lovelacePaidTo (sSeller sale) $ sellerGets numToken

    totalRoyalty :: Integer -> [(PubKeyHash, Integer)]
    totalRoyalty numToken= royaltyAmount (rBeneficiaries royalty) (numToken * sRoyaltyRate sale)

    royaltyDistributed :: Integer -> Bool
    royaltyDistributed numToken = royaltyFor $ totalRoyalty numToken
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
    , spRoyaltyRate :: !Integer
    , spNumToken    :: !Integer -- Number of token to sell
    }

putOnSale ::
  forall s. HasBlockchainActions s =>
  (Royalty, SaleParams) -> Contract (Last Sale) s Text ()
putOnSale (royalty, sp) = do
  m <- findRoyalty royalty

  case m of
    Nothing -> throwError "Royalty not found"

    Just (oref, o, ddt@DelegateDatum{..}) | available >= spNumToken sp -> do

      pkh <- pubKeyHash <$> Contract.ownPubKey
      osc <- mapError (pack . show) (forgeContract pkh [(emptyTokenName, 1)] :: Contract (Last Sale) s CurrencyError OneShotCurrency)
      let
        cs = Currency.currencySymbol osc
        sale = Sale {
              sSeller      = pkh,
              sRoyaltyRate = spRoyaltyRate sp,
              sNFT         = cs
          }
        toReturn = available - spNumToken sp
        -- val1 = assetClassValue (rToken royalty) toReturn <> assetClassValue (royaltyAsset royalty) 1 -- For the delegate script
        -- val3 = Ada.lovelaceValueOf (spNumToken sp * costPrice )

        -- val1: For Delegate script, val2: For creator
        (val1, val3) = if available == spNumToken sp then
                        (token, ada <> nft)          else
                        (token <> nft , ada)
                        where
                          token = assetClassValue (rToken royalty) toReturn
                          nft = assetClassValue (royaltyAsset royalty) 1
                          ada = Ada.lovelaceValueOf (spNumToken sp * costPrice)
                          
        val2 = assetClassValue (rToken royalty) (spNumToken sp) <> assetClassValue (AssetClass (cs, emptyTokenName)) 1 -- For Sale script


        lookups = Constraints.otherScript (saleValidator royalty sale) <>
                  Constraints.otherScript (delegateValidator royalty)  <>
                  Constraints.unspentOutputs ( Map.singleton oref o)

        tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toData (Take (spNumToken sp))) <>
                  Constraints.mustPayToOtherScript (validatorHash $ saleValidator royalty sale)
                      (Datum $ PlutusTx.toData $ SaleDatum $ spSellingPrice sp) val2                          <>
                  Constraints.mustPayToOtherScript (validatorHash $ delegateValidator royalty)
                      (Datum $ PlutusTx.toData $ SaleDatum $ spSellingPrice sp)    val1                       <>
                  Constraints.mustPayToPubKey (rCreator royalty) val3
      tell $ Last $ Just sale
      ledgerTx <- submitTxConstraintsWith @Selling lookups tx
      awaitTxConfirmed $ txId ledgerTx
      logInfo @String $ "Kept token " ++ show (rToken royalty) ++ "for the sale at price " ++ show (spSellingPrice sp)

      where
        available :: Integer
        available = assetClassValueOf (txOutValue $ txOutTxOut o) (rToken royalty)






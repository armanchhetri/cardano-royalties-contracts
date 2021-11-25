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

import           Funds
import           Prelude                   (Semigroup (..), (<$>))
import           Delegate



data Sale = Sale
    {
          sSeller      :: !PubKeyHash  -- Marketplace wallet who wants to sell token
        , sRoyaltyRate :: !Integer     -- Amount in lovelaces the market place is willing to pay as royalty for each token sold
                                       -- which is later to be distributed to the wallet specified by the creator
    }

newtype SaleDatum = SaleDatum {salePrice :: Integer}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''SaleDatum
PlutusTx.unstableMakeIsData ''SaleDatum

data SaleRedeemer = Buy Integer | UpdatePrice | RetrieveSale
  deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''SaleRedeemer
PlutusTx.makeLift ''SaleRedeemer

{-# INLINABLE royaltyAmount #-}
royaltyAmount :: [(PubKeyHash, Integer)] -> Integer -> [(PubKeyHash, Integer)]
royaltyAmount beneficiaries total = [(pkh, f percent ) | (pkh, percent) <- beneficiaries]
                                where 
                                    f :: Integer -> Integer
                                    f  percent = round $ (total * percent) % 100

-- {-# INLINABLE royaltyAmount' #-}
-- royaltyAmount [] _ = []
-- royaltyAmount ((pkh, royaltyPercent):xs) total =
--     (pkh, amount * royaltyPercent `div` 100) : royaltyAmount xs (amount - (amount * royaltyRate `div` 100))

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
        pkhValue = assetClassValueOf (valuePaidTo info pkh) (AssetClass (adaSymbol, adaToken))
      in
        pkhValue >= amt



-- Testing the Royalty 
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Spec.Trace
  (tests,
  testyTest)
  where


import           Control.Lens

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Ledger
import           Ledger.Value               as Value
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract hiding (when)
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (Semigroup(..))
import           Wallet.Emulator
import           Plutus.Contract.Test
import           Test.Tasty

import           Delegate
import           Sale

assetSymbol :: CurrencySymbol
assetSymbol = "f668"

assetToken :: TokenName
assetToken = "BOOK"


test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace1

emCfg :: EmulatorConfig
emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet 1, v1), (Wallet 2, v2), (Wallet 3, v3)]
  where
    v1 :: Value
    v1 = Ada.lovelaceValueOf 50 <> Value.singleton assetSymbol assetToken 500
    v2 :: Value
    v2 = Ada.lovelaceValueOf 1_000_000
    v3 :: Value
    v3 = Ada.lovelaceValueOf 1_000_000   


myTrace1 :: EmulatorTrace ()
myTrace1 = do
    let rpTok = AssetClass (assetSymbol, assetToken)
        pkh1 = pubKeyHash $ walletPubKey $ Wallet 1
        pkh2 = pubKeyHash $ walletPubKey $ Wallet 2
        rp = RoyaltyParams
            {
              rpNumToken      = 150
            , rpPrice         = 100
            , rpSelfPercent   = 50
            , rpBeneficiaries = [(pkh2, 50), (pkh1, 50)]
            , rpToken         = rpTok                
            , rpDeadline      = 20
            }

    h1 <- activateContractWallet (Wallet 1) $ cEndpoint
    -- h1 <- activateContractWallet (Wallet 1) $ createRoyalty rp
    callEndpoint @"create" h1 rp 
    
    void $ Emulator.waitNSlots 2

    r <- getRoyalty h1
    Extras.logInfo $ "The royalty is " ++ show r

    h1' <- activateContractWallet (Wallet 1) $ dEndpoints r

    void $ Emulator.waitNSlots 5
    callEndpoint @"update" h1' 200

    void $ Emulator.waitNSlots 5
    callEndpoint @"retrieve" h1' 50


    void $ Emulator.waitNSlots 2

    let sp = SaleParams{
        spSellingPrice = 300
      , spRoyaltyRate  = 50
      , spNumToken     = 40
    }

    h2 <- activateContractWallet (Wallet 2) $ pEndpoint r

    callEndpoint @"putOnSale" h2 sp 

    void $ Emulator.waitNSlots 2
    s <- getSale h2
    Extras.logInfo $ "The sale is " ++ show s

    h2' <- activateContractWallet (Wallet 2) $ sEndpoints (r,s)
    
    h3 <- activateContractWallet (Wallet 3) $ sEndpoints (r,s)

    void $ Emulator.waitNSlots 2

    callEndpoint @"updateSale" h2' 200

    void $ Emulator.waitNSlots 2

    callEndpoint @"retrieveSale" h2' 10

    void $ Emulator.waitNSlots 2

    callEndpoint @"buy" h3 20


    void $ Emulator.waitNSlots 1


    where
    getRoyalty :: ContractHandle (Last Royalty) CreateSchema Text -> EmulatorTrace Royalty
    getRoyalty h = do
        l <- observableState h
        case l of
            Last Nothing       -> Emulator.waitNSlots 1 >> getRoyalty h
            Last (Just royalty) -> Extras.logInfo (show royalty) >> return royalty



    getSale :: ContractHandle (Last Sale) CreateSaleSchema Text -> EmulatorTrace Sale
    getSale h = do
      l <- observableState h
      case l of
           Last Nothing       -> Emulator.waitNSlots 1 >> getSale h
           Last (Just sale) -> Extras.logInfo (show sale) >> return sale


-- Tasty package
tests :: TestTree
tests = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "Royalty trace test"
    (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf   8_500  <> Value.singleton assetSymbol assetToken (-100))
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf (-4_500) <> Value.singleton assetSymbol assetToken  10)
     .&&. walletFundsChange (Wallet 3) (Ada.lovelaceValueOf (-4000) <> Value.singleton assetSymbol assetToken  20)
    )
    myTrace1



testyTest :: IO ()
testyTest = defaultMain tests 
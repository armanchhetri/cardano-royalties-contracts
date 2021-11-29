-- Testing the Royalty 
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Trace where

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

import           Delegate
import           Sale

assetSymbol :: CurrencySymbol
assetSymbol = "f668"

assetToken :: TokenName
assetToken = "BOOK"

test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    -- Initial config
    emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet 1, v1), (Wallet 2, v2), (Wallet 3, v3)]

    v1 :: Value
    v1 = Ada.lovelaceValueOf 1_000_000 <> Value.singleton assetSymbol assetToken 500

    v2 :: Value
    v2 = Ada.lovelaceValueOf 1_000_000

    v3 :: Value
    v3 = Ada.lovelaceValueOf 1_000_000


myTrace :: EmulatorTrace ()
myTrace = do
    let rpTok = AssetClass (assetSymbol, assetToken)
        pkh2 = pubKeyHash $ walletPubKey $ Wallet 2
        rp = RoyaltyParams
            {
              rpNumToken      = 150
            , rpPrice         = 100
            , rpSelfPercent   = 50
            , rpBeneficiaries = [(pkh2, 50)]
            , rpToken         = rpTok                
            , rpDeadline      = 20
            }

    h1 <- activateContractWallet (Wallet 1) $ createRoyalty rp
    
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

    h2 <- activateContractWallet (Wallet 2) $ putOnSale (r,sp)

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


    void $ Emulator.waitNSlots 5


    where
    getRoyalty :: ContractHandle (Last Royalty) BlockchainActions Text -> EmulatorTrace Royalty
    getRoyalty h = do
        l <- observableState h
        case l of
            Last Nothing       -> Emulator.waitNSlots 1 >> getRoyalty h
            Last (Just royalty) -> Extras.logInfo (show royalty) >> return royalty



    getSale :: ContractHandle (Last Sale) BlockchainActions Text -> EmulatorTrace Sale
    getSale h = do
      l <- observableState h
      case l of
           Last Nothing       -> Emulator.waitNSlots 1 >> getSale h
           Last (Just sale) -> Extras.logInfo (show sale) >> return sale

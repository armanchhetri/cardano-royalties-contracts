{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Spec.Model
    -- ( tests
    -- , test
    -- , RModel (..)
    -- ) 
    where

import           Control.Lens                       hiding (elements)
import           Control.Monad                      (void, when)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust, isNothing)
import           Data.Monoid                        (Last (..))
import           Data.String                        (IsString (..))
import           Data.Text                          (Text)
import           Plutus.Contract.Test
import           Plutus.Contract.Test.ContractModel
import           Plutus.Trace.Emulator
import           Ledger                             hiding (singleton)
import           Ledger.Ada                         as Ada
import           Ledger.Value
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Plutus.Contract                    hiding (when, waitNSlots)

import           Delegate hiding (DelegateRedeemer (..))
import           Sale hiding (SaleRedeemer (..))

-- import           Week08.TokenSale                   (TokenSale (..), TSStartSchema', TSUseSchema, startEndpoint', useEndpoints, nftName)

data State = State
    { _sPrice    :: !Integer 
    , _sToken    :: !Integer
    , _sLovelace :: !Integer
    } deriving Show

makeLenses ''State

data RState = RState
    { _dState :: !State --For Delegate
    , _sState :: !State --For Sale
    } deriving Show

makeLenses ''RState

newtype RModel = RModel {_rModel :: Map Wallet RState}
    deriving Show

makeLenses ''RModel

tests :: TestTree
tests = testProperty "royalty model" prop_R

instance ContractModel RModel where

    data Action RModel =
              Create       Wallet Integer Integer
            | UpdatePrice  Wallet Wallet  Integer
            | Retrieve     Wallet Wallet  Integer
            | Sell         Wallet Wallet  Integer Integer -- [numOfToken, SellingPrice]
            | RetrieveSale Wallet Wallet  Integer
            | UpdateSale   Wallet Wallet  Integer
            | Buy          Wallet Wallet  Integer
        deriving (Show, Eq)

    data ContractInstanceKey RModel w s e where
        CreateKey    :: Wallet           -> ContractInstanceKey RModel (Last Royalty) CreateSchema' Text
        DelegateKey  :: Wallet -> Wallet -> ContractInstanceKey RModel ()             DelegateSchema    Text
        PutOnSaleKey :: Wallet -> Wallet -> ContractInstanceKey RModel (Last Sale)    CreateSaleSchema' Text
        SaleKey      :: Wallet -> Wallet -> ContractInstanceKey RModel ()             SaleSchema        Text

    -- instanceTag key _ = fromString $ "instance tag for: " ++ show key

    arbitraryAction _ = oneof $
        (Create        <$> genWallet <*> genPrice <*> genToken):
        [ UpdatePrice  <$> genWallet <*> genWallet <*> genPrice ] ++
        [ Retrieve     <$> genWallet <*> genWallet <*> genToken ] ++
        [ Sell         <$> genWallet <*> genWallet <*> genToken <*> genPrice] ++
        [ RetrieveSale <$> genWallet <*> genWallet <*> genToken ] ++
        [ UpdateSale   <$> genWallet <*> genWallet <*> genPrice ] ++
        [ Buy          <$> genWallet <*> genWallet' <*> genToken ]

    initialState = RModel Map.empty

    nextState (Create w p n) = do
        let token = tokens Map.! w 
        withdraw w $ assetClassValue token n  -- Take away tokens from wallet
        let s1 = State p n 0
            s2 = State 0 0 0
            rState = RState s1 s2
        (rModel . at w) $= Just rState

        -- bc <- askModelState $ view $ balanceChange w
        -- let token = tokens Map.! w 
        -- when (tokenAmt + assetClassValueOf bc token > n) $ do --Check if enough tokens available
        --     withdraw w $ assetClassValue token n  -- Take away tokens from wallet
        --     let s1 = State p n 0
        --         s2 = State 0 0 0
        --         rState = RState s1 s2
        --     (rModel . at w) $= Just rState
        wait 1

    nextState (UpdatePrice v w p) = do
        started <- hasStarted v      
        when (v == w && started) $
            (rModel . ix v . dState . sPrice) $= p
        wait 1

    nextState (Retrieve v w n) = do
        m <- getRState v
        case m of
            Just r 
                | (r ^. dState . sToken >= n && v == w) -> do
                    (rModel . ix v . dState . sToken) $~ (+(-n))

            _ -> return ()
        wait 1

    nextState (Sell v w n p') = do -- v is creator and w is seller
        when (n > 0 && p' > 0) $ do
            m <- getRState v
            case m of
                Just r
                    | r ^. dState . sToken >= n -> do
                        let p = r ^. dState . sPrice 
                            l = p * n
                            s1 = State 0 0 0
                            s2 = State p' n 0
                            rState = RState s1 s2
                        (rModel . at w) $= Just rState
                        withdraw w $ lovelaceValueOf l
                        deposit v $ lovelaceValueOf l
                        (rModel . ix v . dState . sToken) $~ (+(- n))
                        
                        
                _ -> return ()
        wait 1

    nextState (RetrieveSale v w n ) = do
        when (n > 0 && v == w) $ do
            m <- getRState w
            case m of
                Just r
                    | r ^. sState . sToken >= n -> do
                        deposit w $ assetClassValue (tokens Map.! w) n
                        (rModel . ix w . sState . sToken) $~ (+ (- n))
                        
                _ -> return ()
        wait 1

    nextState (UpdateSale v w p) = do
        when (p > 0 && v == w) $ do
            m <- getRState w
            case m of
                Just r
                    | r ^. sState . sToken > 0 -> do -- Check is to ensure that sale has started
                        (rModel . ix w . sState . sPrice) $= p
                        
                _ -> return ()
        wait 1


    nextState (Buy v w n) = do -- w is buying wallet v is marketplace wallet
        when (n > 0) $ do
            m <- getRState v
            case m of
                Just r
                    | r ^. sState . sToken >= n -> do
                        let p = r ^. sState . sPrice
                            l = p * n
                        withdraw w $ lovelaceValueOf l
                        deposit w $ assetClassValue (tokens Map.! v) n
                        (rModel . ix v . sState . sToken) $~ (+(- n))
                _ -> return ()
        wait 1


    perform h _ cmd = case cmd of
        (Create w p n)       -> callEndpoint @"create"       (h $ CreateKey w) (getRoyaltyParams w n p, nftCurrencies Map.! w)     >> delay 1
        (UpdatePrice v w p)  -> callEndpoint @"update"       (h $ DelegateKey v w) p                        >> delay 1
        (Retrieve v w n)     -> callEndpoint @"retrieve"     (h $ DelegateKey v w) n                        >> delay 1
        (Sell v w n p)       -> callEndpoint @"putOnSale"    (h $ PutOnSaleKey v w) (getSaleParams w n p, nftCurrencies Map.! w)  >> delay 1
        (RetrieveSale v w n) -> callEndpoint @"retrieveSale" (h $ SaleKey v w) n                            >> delay 1
        (UpdateSale v w p)   -> callEndpoint @"updateSale"   (h $ SaleKey v w) p                            >> delay 1
        (Buy v w n)          -> callEndpoint @"buy"          (h $ SaleKey v w) n                            >> delay 1

    precondition s (Create w _ _)          = isNothing $ getRState' s w
    precondition s (UpdatePrice v _ _)     = isJust    $ getRState' s v
    precondition s (Retrieve v _ _)        = isJust    $ getRState' s v
    precondition s (Sell v w _ p)       = (isJust   $ getRState' s v) && p > royRate Map.! w
    precondition s (RetrieveSale v _ _)    = hasSaleStarted v s
    precondition s (UpdateSale v _ _)      = hasSaleStarted v s
    precondition s (Buy v _ _)             = hasSaleStarted v s

deriving instance Eq (ContractInstanceKey RModel w s e)
deriving instance Show (ContractInstanceKey RModel w s e)

getRState' :: ModelState RModel -> Wallet -> Maybe RState
getRState' s v = s ^. contractState . rModel . at v

getRState :: Wallet -> Spec RModel (Maybe RState)
getRState v = do
    s <- getModelState
    return $ getRState' s v

hasStarted :: Wallet -> Spec RModel Bool
hasStarted v = isJust <$> getRState v

hasSaleStarted :: Wallet -> ModelState RModel -> Bool 
hasSaleStarted v s = case getRState' s v of
    Nothing ->  False 
    Just saleState  ->  saleState ^. sState . sToken /= 0

w1, w2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2

wallets :: [Wallet]
wallets = [w1, w2]

tokenCurrencies, nftCurrencies :: Map Wallet CurrencySymbol
tokenCurrencies = Map.fromList $ zip wallets ["aa", "bb"]
nftCurrencies   = Map.fromList $ zip wallets ["01", "02"]

tokenNames :: Map Wallet TokenName
tokenNames = Map.fromList $ zip wallets ["DOLLAR", "RUPEE"]

tokens :: Map Wallet AssetClass
tokens = Map.fromList [(w, AssetClass (tokenCurrencies Map.! w, tokenNames Map.! w)) | w <- wallets]

beneficiary :: [(PubKeyHash, Integer)]
beneficiary = [(pubKeyHash $ walletPubKey w1, 50), (pubKeyHash $ walletPubKey w2, 50)]

getRoyaltyParams :: Wallet -> Integer -> Integer -> RoyaltyParams
getRoyaltyParams w n p = RoyaltyParams
                    {
                      rpNumToken      = n
                    , rpPrice         = p
                    , rpSelfPercent   = 50
                    , rpBeneficiaries = beneficiary
                    , rpToken         = tokens Map.! w              
                    , rpDeadline      = 20
                    }
               

getSaleParams:: Wallet -> Integer -> Integer -> SaleParams
getSaleParams w n p = SaleParams
                        {
                          spSellingPrice = p
                        , spRoyaltyRate  = royRate Map.! w
                        , spNumToken     = n
                        }


nftAssets :: Map Wallet AssetClass
nftAssets = Map.fromList [(w, AssetClass (nftCurrencies Map.! w, emptyTokenName)) | w <- wallets]

nfts :: Map Wallet Value
nfts = Map.fromList [(w, assetClassValue (nftAssets Map.! w) 1) | w <- wallets]

royRate :: Map Wallet Integer
royRate = Map.fromList [(w1, 100), (w2, 200) ]

rs :: Map Wallet Royalty
rs = Map.fromList
    [ (w, Royalty {   rCreator =  pubKeyHash $ walletPubKey w
                    , rToken  =  tokens Map.! w
                    , rNFT    =  nftCurrencies Map.! w
                    , rBeneficiaries = beneficiary
                    , rDeadline = 20
                    })
    | w <- wallets
    ]



ss :: Map Wallet Sale 
ss = Map.fromList
    [ (w, Sale {      sSeller =  pubKeyHash $ walletPubKey w
                    , sRoyaltyRate  =  royRate Map.! w
                    , sNFT    =  nftCurrencies Map.! w
                    
                    })
    | w <- wallets
    ]



delay :: Int -> EmulatorTrace ()
delay = void . waitNSlots . fromIntegral

instanceSpec :: [ContractInstanceSpec RModel]
instanceSpec =
    [ContractInstanceSpec (CreateKey w) w cEndpoint' | w <- wallets] ++
    [ContractInstanceSpec (DelegateKey w v) w $ dEndpoints $ rs Map.! w | w <- wallets, v <- wallets] ++
    [ContractInstanceSpec (PutOnSaleKey w v) w $ pEndpoint' $ rs Map.! w | w <- wallets, v <- wallets] ++
    [ContractInstanceSpec (SaleKey w v) w $ sEndpoints $ (rs Map.! w, ss Map.! v ) | w <- wallets, v <- (wallets' ++ wallets)]

genWallet :: Gen Wallet
genWallet = elements wallets


wallets' :: [Wallet]
wallets' = [Wallet 3, Wallet 4]

genWallet' :: Gen Wallet
genWallet' = elements wallets'


genPrice :: Gen Integer
genPrice = chooseInteger (100, 300)

genToken :: Gen Integer
genToken = chooseInteger (300, 600)

-- genNonNeg :: Gen Integer
-- genNonNeg = chooseInteger ()

tokenAmt :: Integer
tokenAmt = 1_000

prop_R :: Actions RModel -> Property
prop_R = withMaxSuccess 100 . propRunActionsWithOptions
    (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left d))
    instanceSpec
    (const $ pure True)
  where
    d :: InitialDistribution
    d = Map.fromList $ [ ( w
                         , lovelaceValueOf 1000_000_000 <>
                           (nfts Map.! w)               <>
                           mconcat [assetClassValue t tokenAmt | t <- Map.elems tokens])
                       | w <- wallets
                       ]

test :: IO ()
test = quickCheck prop_R

samples = sample (arbitrary :: Gen (Actions RModel))
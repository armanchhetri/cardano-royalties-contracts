module Main
    ( main
    ) where

import qualified Spec.Model
import qualified Spec.Trace

import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Royalty"
    [ Spec.Trace.tests
    , Spec.Model.tests
    ]

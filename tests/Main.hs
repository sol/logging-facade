module Main (main) where

import           Test.Framework (defaultMain)
import           Test.Framework.Providers.DocTest

import qualified DynamicSpecTest

main = do
  doctests <- docTest ["Util", "DynamicSpec"] ["-i../src"]
  defaultMain [doctests, DynamicSpecTest.tests]

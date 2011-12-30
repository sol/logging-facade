module Main (main) where

import           Test.Framework (defaultMain)
import           Test.Framework.Providers.DocTest

main = do
  doctests <- docTest ["Util"] ["-i../src"]
  defaultMain [doctests]

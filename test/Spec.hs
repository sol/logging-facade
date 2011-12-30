{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
module Spec (main, spec) where

import           Test.Hspec.ShouldBe

import           Control.Exception
import           Prelude hiding (catch, error, undefined)
import           Logging

deriving instance Eq ErrorCall

main = hspec spec

spec = do
  describe "undefined" $ do
    it "includes the source location" $ do
      e <- $(undefined) `catch` return
      e `shouldBe` ErrorCall "undefined (Spec.hs:18)"

  describe "error" $ do
    it "includes the source location" $ do
      e <- $(error "foo") `catch` return
      e `shouldBe` ErrorCall "foo (Spec.hs:23)"

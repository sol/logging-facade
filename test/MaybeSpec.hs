{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module MaybeSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Logging

maybeWithLogging :: MaybeT Logging a -> (Maybe a, [String])
maybeWithLogging = fmap (map $ ($ "") . logMessage) . withLogging . runMaybeT

main = hspec spec

spec = do
  describe "withLogging" $ do
    it "extracts a value and a log" $ do
      (a, [m]) <- return . withLogging $ do
        $(logError "foo")
        return (23 :: Int)
      a `shouldBe` 23
      logMessage m "" `shouldBe` "foo"

  describe "maybeWithLogging" $ do
    it "can extract Nothing and a log" $ do
      maybeWithLogging $ do
        $(logError "foo")
        mzero
      `shouldBe` (Nothing :: Maybe Int, ["foo"])

    it "can extract Just a value and a log" $ do
      maybeWithLogging $ do
        $(logError "foo")
        return (23 :: Int)
      `shouldBe` (Just 23, ["foo"])

    it "keeps log messages of partially evaluated actions that yield Nothing" $ do
      maybeWithLogging $ do
        $(logError "foo")
        x <- (lift . runMaybeT) $ do
          $(logError "bar")
          mzero
        return (23 :: Int, x)
      `shouldBe` (Just (23, Nothing :: Maybe Int), ["foo", "bar"])

    it "keeps log messages of partially evaluated actions that yield Just a value" $ do
      maybeWithLogging $ do
        $(logError "foo")
        x <- (lift . runMaybeT) $ do
          $(logError "bar")
          return (23 :: Int)
        return (23 :: Int, x)
      `shouldBe` (Just (23, Just 23), ["foo", "bar"])

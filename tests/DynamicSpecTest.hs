{-# LANGUAGE TemplateHaskell #-}
module DynamicSpecTest (tests) where
import Test.Framework.TH (testGroupGenerator)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Test.HUnit
import Test.QuickCheck

import DynamicSpec

import Text.ParserCombinators.ReadP

tests = $(testGroupGenerator)

instance Arbitrary Literal where
  arbitrary = do
    s <- arbitrary `suchThat` (not . elem '{')
    return (Literal s)

instance Arbitrary Capture where
  arbitrary = do
    s <- arbitrary `suchThat` (not . null) `suchThat` (not . elem '}')
    return (Capture s)

prop_parse t = (parse . render) t == Just t
  where
    render (Literal s, x) = s ++ render_ x
      where
        render_ [] = ""
        render_ ((Capture c, Literal s) : xs) = concat ["{", c, "}", s, render_ xs]

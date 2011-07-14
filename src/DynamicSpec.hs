{-# LANGUAGE TemplateHaskell #-}
module DynamicSpec where

import           Language.Haskell.TH

import           Data.Maybe
import           Data.Typeable
import           Control.Applicative hiding (many)

import           Data.Text   (Text)
import qualified Data.Text as Text

import           Text.ParserCombinators.ReadP

-- | Format given string.
--
-- Identifiers in curly braces are substitute from the outer scope (dynamically
-- scoped).
--
-- Example:
--
-- >>> :set -fth
-- >>> import Data.Text as T
-- >>> let x = "bar"
-- >>> let y = 23
-- >>> T.concat $(format "foo {x} {y} baz")
-- "foo bar 23 baz"
format :: String -> Q Exp
format s =
  case parse s of
    Nothing -> fail ("Invalid format string: " ++ show s)
    Just (Literal x, xs) -> [|Text.pack x : $(format_ xs)|]
  where
    format_ [] = [|[]|]
    format_ ((Capture c, Literal x) : xs) = [|formatValue $(dyn c) : Text.pack x : $(format_ xs)|]

formatValue :: (Typeable a, Show a) => a -> Text
formatValue a = fromMaybe (Text.pack $ show a) (formatValue_ a)

-- | Special formating rules for Text and String
formatValue_ :: Typeable a => a -> Maybe Text
formatValue_ a = (cast a) <|> Text.pack <$> cast a

type Spec = (Literal, [(Capture, Literal)])
newtype Literal = Literal String deriving (Eq, Show)
newtype Capture = Capture String deriving (Eq, Show)

-- | Parse a format strig.
parse :: String -> Maybe Spec
parse s =
  case readP_to_S spec s of
    [(x, "")] -> Just x
    _         -> Nothing

spec :: ReadP Spec
spec = do
  l <- literal
  c <- many capture
  eof
  return (l, c)

literal :: ReadP Literal
literal = do
  s <- munch (/= '{')
  return (Literal s)

capture :: ReadP (Capture, Literal)
capture = do
  _ <- char '{'
  c <- munch1 (/= '}')
  _ <- char '}'
  l <- literal
  return (Capture c, l)

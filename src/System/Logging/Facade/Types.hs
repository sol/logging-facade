module System.Logging.Facade.Types where

data LogLevel = TRACE | DEBUG | INFO | WARN | ERROR
  deriving (Eq, Show, Ord, Bounded, Enum)

data Location = Location {
  locationFile :: String
, locationPackage :: String
, locationModule :: String
, locationLine :: Int
, locationColumn :: Int
} deriving (Eq, Show)

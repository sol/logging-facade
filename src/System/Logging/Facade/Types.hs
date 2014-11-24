module System.Logging.Facade.Types where

data LogLevel = TRACE | DEBUG | INFO | WARN | ERROR
  deriving (Eq, Show, Ord, Bounded, Enum)

data Location = Location {
  locationPackage :: String
, locationModule :: String
, locationFile :: String
, locationLine :: Int
, locationColumn :: Int
} deriving (Eq, Show)

data LogRecord = LogRecord {
  logRecordLevel :: LogLevel
, logRecordLocation :: Maybe Location
, logRecordMessage :: String
} deriving (Eq, Show)

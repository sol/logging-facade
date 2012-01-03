module Logging.Type where

data LogLevel = TRACE | DEBUG | INFO | WARN | ERROR
  deriving (Eq, Show)

data Location = Location {
  locationFilename  :: String
, locationPackage   :: String
, locationModule    :: String
, locationLine      :: Int
, locationColumn    :: Int
}

data LogRecord = LogRecord {
  logLevel    :: LogLevel
, logMessage  :: ShowS
, logLocation :: Location
}

class (Monad m) => MonadLogging m where
  consumeLogRecord :: LogRecord -> m ()

logTraceS, logDebugS, logInfoS, logWarnS, logErrorS :: (MonadLogging m) => Location -> ShowS -> m ()
logTraceS loc m = consumeLogRecord (LogRecord TRACE m loc)
logDebugS loc m = consumeLogRecord (LogRecord DEBUG m loc)
logInfoS  loc m = consumeLogRecord (LogRecord INFO  m loc)
logWarnS  loc m = consumeLogRecord (LogRecord WARN  m loc)
logErrorS loc m = consumeLogRecord (LogRecord ERROR m loc)

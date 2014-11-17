-- |
-- This module is intended to be imported qualified:
--
-- > import qualified System.Logging.Facade as Log
module System.Logging.Facade (
-- * Producing log messages
  log
, trace
, debug
, info
, warn
, error

-- * Types
, Logging
, LogLevel(..)
) where

import           Prelude hiding (log, error)

import           System.Logging.Facade.Types
import           System.Logging.Facade.Class

-- | Produce a log message with specified log level.
log :: Logging m => LogLevel -> String -> m ()
log level message = consumeLogRecord (LogRecord level Nothing message)

-- | Produce a log message with log level `TRACE`.
trace :: Logging m => String -> m ()
trace = log TRACE

-- | Produce a log message with log level `DEBUG`.
debug :: Logging m => String -> m ()
debug = log DEBUG

-- | Produce a log message with log level `INFO`.
info :: Logging m => String -> m ()
info = log INFO

-- | Produce a log message with log level `WARN`.
warn :: Logging m => String -> m ()
warn = log WARN

-- | Produce a log message with log level `ERROR`.
error :: Logging m => String -> m ()
error = log ERROR

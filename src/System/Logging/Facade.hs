{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
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
import           Data.CallStack

import           System.Logging.Facade.Types
import           System.Logging.Facade.Class

-- | Produce a log message with specified log level.
log :: (HasCallStack, Logging m) => LogLevel -> String -> m ()
log level message = consumeLogRecord (LogRecord level location message)

location :: HasCallStack => Maybe Location
location = case reverse callStack of
  (_, loc) : _ -> Just $ Location (srcLocPackage loc) (srcLocModule loc) (srcLocFile loc) (srcLocStartLine loc) (srcLocStartCol loc)
  _ -> Nothing

-- | Produce a log message with log level `TRACE`.
trace :: (HasCallStack, Logging m) => String -> m ()
trace = log TRACE

-- | Produce a log message with log level `DEBUG`.
debug :: (HasCallStack, Logging m) => String -> m ()
debug = log DEBUG

-- | Produce a log message with log level `INFO`.
info :: (HasCallStack, Logging m) => String -> m ()
info = log INFO

-- | Produce a log message with log level `WARN`.
warn :: (HasCallStack, Logging m) => String -> m ()
warn = log WARN

-- | Produce a log message with log level `ERROR`.
error :: (HasCallStack, Logging m) => String -> m ()
error = log ERROR

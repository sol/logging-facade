{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,8,1)
#define HAS_SOURCE_LOCATIONS
{-# LANGUAGE ImplicitParams #-}
#endif
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
, LogLevel(..)
) where

import           Prelude hiding (log, error)

import           System.Logging.Facade.Types
import           System.Logging.Facade.Sink

#ifdef HAS_SOURCE_LOCATIONS
#if ! MIN_VERSION_base(4,9,0)
import           GHC.SrcLoc
#endif
import           GHC.Stack
#define with_loc (?loc :: CallStack) =>
#else
#define with_loc
#endif

-- | Produce a log message with specified log level.
log :: with_loc LogLevel -> String -> IO ()
log level message = do
  sink <- getLogSink
  sink (LogRecord level location message)
  where
    location :: Maybe Location
#ifdef HAS_SOURCE_LOCATIONS
    location = case reverse (getCallStack ?loc) of
      (_, loc) : _ -> Just $ Location (srcLocPackage loc) (srcLocModule loc) (srcLocFile loc) (srcLocStartLine loc) (srcLocStartCol loc)
      _ -> Nothing
#else
    location = Nothing
#endif

-- | Produce a log message with log level `TRACE`.
trace :: with_loc String -> IO ()
trace = log TRACE

-- | Produce a log message with log level `DEBUG`.
debug :: with_loc String -> IO ()
debug = log DEBUG

-- | Produce a log message with log level `INFO`.
info :: with_loc String -> IO ()
info = log INFO

-- | Produce a log message with log level `WARN`.
warn :: with_loc String -> IO ()
warn = log WARN

-- | Produce a log message with log level `ERROR`.
error :: with_loc String -> IO ()
error = log ERROR

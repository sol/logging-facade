{-# LANGUAGE TemplateHaskell #-}
module Logging.HsLogger (hsLoggerSink) where

import           Logging
import qualified System.Log.Logger as HsLogger
import           Text.Format

hsLoggerSink :: LogRecord -> IO ()
hsLoggerSink m = HsLogger.logM channel level $(format "{linfo} - {message}")
  where
    channel  = logChannel m
    message = logMessage m
    linfo = formatLocation $ logLocation m
    level = case logLevel m of
      TRACE -> HsLogger.DEBUG
      DEBUG -> HsLogger.DEBUG
      INFO  -> HsLogger.INFO
      WARN  -> HsLogger.WARNING
      ERROR -> HsLogger.ERROR

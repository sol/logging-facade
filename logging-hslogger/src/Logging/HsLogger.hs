{-# LANGUAGE OverloadedStrings #-}
module Logging.HsLogger (hsLoggerSink) where

import qualified Data.Text as Text
import           Logging
import qualified System.Log.Logger as HsLogger

hsLoggerSink :: LogRecord -> IO ()
hsLoggerSink m = HsLogger.logM (Text.unpack $ logChannel m) (mapLevel $ logLevel m) (Text.unpack $ Text.concat $ logLocationInfo m : " - " : logMessage m)
  where
    mapLevel TRACE = HsLogger.DEBUG
    mapLevel DEBUG = HsLogger.DEBUG
    mapLevel INFO  = HsLogger.INFO
    mapLevel WARN  = HsLogger.WARNING
    mapLevel ERROR = HsLogger.ERROR

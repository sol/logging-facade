module Logging.HsLogger (hsLoggerSink) where

import           Logging
import qualified System.Log.Logger as HsLogger

hsLoggerSink :: LogRecord -> IO ()
hsLoggerSink m = HsLogger.logM (logChannel m) (mapLevel $ logLevel m) (logLocationInfo m ++ " - " ++ logMessage m [])
  where
    mapLevel TRACE = HsLogger.DEBUG
    mapLevel DEBUG = HsLogger.DEBUG
    mapLevel INFO  = HsLogger.INFO
    mapLevel WARN  = HsLogger.WARNING
    mapLevel ERROR = HsLogger.ERROR

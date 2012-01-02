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

logChannel :: LogRecord -> String
logChannel m = $(format "{package}.{module_}")
  where
    loc = logLocation m
    package = stripVersion $ locationPackage loc
    module_ = locationModule loc

-- | Strip version string from given package name.
--
-- The package name @main@ is returned verbatim.  If the package name is not
-- @main@, we assume that there is always a version string, delimited with a
-- @\'-\'@ from the package name.  Behavior is unspecified for package names
-- that are neither @main@ nor have a version string.
--
-- Examples:
--
-- >>> stripVersion "main"
-- "main"
--
-- >>> stripVersion "foo-0.0.0"
-- "foo"
--
-- >>> stripVersion "foo-bar-0.0.0"
-- "foo-bar"
stripVersion :: String -> String
stripVersion p = case p of
  "main" -> p
  _ -> reverse $ tail $ dropWhile (/= '-') $ reverse p

module System.Logging.Facade.HsLogger (hsLoggerSink) where

import qualified System.Log.Logger as HsLogger
import           System.Logging.Facade.Types

hsLoggerSink :: String -> LogRecord -> IO ()
hsLoggerSink defaultChannel record = HsLogger.logM channel level (message "")
  where
    mLocation = logRecordLocation record
    channel = maybe defaultChannel channelFromLocation mLocation

    level = case logRecordLevel record of
      TRACE -> HsLogger.DEBUG
      DEBUG -> HsLogger.DEBUG
      INFO  -> HsLogger.INFO
      WARN  -> HsLogger.WARNING
      ERROR -> HsLogger.ERROR

    message = location . showString " - " . showString (logRecordMessage record)
    location = maybe (showString "") ((showString " " .) . formatLocation) mLocation

formatLocation :: Location -> ShowS
formatLocation loc = showString (locationFile loc) . colon . shows (locationLine loc) . colon . shows (locationColumn loc)
  where colon = showString ":"

channelFromLocation :: Location -> String
channelFromLocation loc = package ++ "." ++ locationModule loc
  where
    package = stripVersion (locationPackage loc)

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

{-# LANGUAGE TemplateHaskell #-}
module Logging (
  logTrace
, logDebug
, logInfo
, logWarn
, logError
, LogRecord
, logChannel
, logLevel
, logMessage
, logLocation
, formatLocation
, LogLevel (..)
, setLogSink
, defaultLogSink
, error
, undefined
) where

import           Prelude hiding (error, undefined)
import qualified Prelude

import           System.IO (hPutStrLn, stderr)

import           Language.Haskell.TH hiding (location)
import           Language.Haskell.TH.Syntax hiding (location)
import qualified Language.Haskell.TH.Syntax as TH

import           Data.IORef
import           Foreign (unsafePerformIO)

import           Text.Format (formatS, format)
import           Util (stripVersion)

-- | Compatibility function for old LogRecord format
logChannel :: LogRecord -> String
logChannel m = let loc = logLocation m in
  (stripVersion $ locationPackage loc) ++ "." ++ locationModule loc

data LogLevel = TRACE | DEBUG | INFO | WARN | ERROR
  deriving (Eq, Show)

instance Lift LogLevel where
  lift TRACE = [|TRACE|]
  lift DEBUG = [|DEBUG|]
  lift INFO  = [|INFO|]
  lift WARN  = [|WARN|]
  lift ERROR = [|ERROR|]

data LogRecord = LogRecord {
  logLevel    :: LogLevel
, logMessage  :: ShowS
, logLocation :: Location
}

data Location = Location {
  locationFilename  :: String
, locationPackage   :: String
, locationModule    :: String
, locationLine      :: Int
, locationColumn    :: Int
}

formatLocation :: Location -> ShowS
formatLocation (Location filename _ _ line _) = $(formatS "{filename}:{line}")

location :: Q Exp
location = do
  loc <- TH.location
  let filename = loc_filename loc
  let package = loc_package loc
  let module_ = loc_module loc
  let (line, column) = loc_start loc
  [|Location filename package module_ line column|]

-- We use the unsafePerformIO hack to share one sink across a process.
logSink :: IORef (LogRecord -> IO ())
{-# NOINLINE logSink #-}
logSink = unsafePerformIO (newIORef defaultLogSink)

setLogSink :: (LogRecord -> IO ()) -> IO ()
setLogSink sink = writeIORef logSink sink

consumeLogRecord :: LogRecord -> IO ()
consumeLogRecord m = do
  sink <- readIORef logSink
  sink m

-- | Write log messages to stderr.
defaultLogSink :: LogRecord -> IO ()
defaultLogSink m =
  hPutStrLn stderr $(format "{level} {linfo}: {message}")
  where
    level = logLevel m
    message = logMessage m
    linfo = formatLocation (logLocation m)

createLogRecord :: LogLevel -> String -> Q Exp
createLogRecord level message = [|LogRecord level $(formatS message) $(location)|]

logTrace, logDebug, logInfo, logWarn, logError :: String -> ExpQ
logTrace message = [| consumeLogRecord $(createLogRecord TRACE message) |]
logDebug message = [| consumeLogRecord $(createLogRecord DEBUG message) |]
logInfo  message = [| consumeLogRecord $(createLogRecord INFO  message) |]
logWarn  message = [| consumeLogRecord $(createLogRecord WARN  message) |]
logError message = [| consumeLogRecord $(createLogRecord ERROR message) |]

emitError :: Location -> ShowS -> a
emitError loc m = Prelude.error $(format "{m} ({linfo})")
  where
    linfo = formatLocation loc

error :: String -> Q Exp
error message = [|emitError $(location) $(formatS message)|]

undefined :: Q Exp
undefined = [|$(error "undefined")|]

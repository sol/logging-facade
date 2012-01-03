{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Logging (
  logTrace
, logDebug
, logInfo
, logWarn
, logError

, LogRecord
, logLevel
, logMessage
, logLocation

, LogLevel (..)

, Location
, locationFilename
, locationPackage
, locationModule
, locationLine
, locationColumn
, formatLocation

, setLogSink
, defaultLogSink

, MonadLogging(..)
, withLogging
, Logging

, error
, undefined
) where

import           Prelude hiding (error, undefined)
import qualified Prelude

import           System.IO (hPutStrLn, stderr)

import           Language.Haskell.TH hiding (location)
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (Lift(..))

import           Data.IORef
import           Foreign (unsafePerformIO)

import           Text.Format (formatS, format)

import           Logging.Type
import           Logging.Maybe

instance Lift LogLevel where
  lift TRACE = [|TRACE|]
  lift DEBUG = [|DEBUG|]
  lift INFO  = [|INFO|]
  lift WARN  = [|WARN|]
  lift ERROR = [|ERROR|]

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

instance MonadLogging IO where
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
logTrace message = [|logTraceS $(location) $(formatS message)|]
logDebug message = [|logDebugS $(location) $(formatS message)|]
logInfo  message = [|logInfoS  $(location) $(formatS message)|]
logWarn  message = [|logWarnS  $(location) $(formatS message)|]
logError message = [|logErrorS $(location) $(formatS message)|]

emitError :: Location -> ShowS -> a
emitError loc m = Prelude.error $(format "{m} ({linfo})")
  where
    linfo = formatLocation loc

error :: String -> Q Exp
error message = [|emitError $(location) $(formatS message)|]

undefined :: Q Exp
undefined = [|$(error "undefined")|]

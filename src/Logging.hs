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
, logLocationInfo
, LogLevel (..)
, setLogSink
, defaultLogSink
, error
, undefined
) where

import           Prelude hiding (error, undefined)
import qualified Prelude

import           System.IO (hPutStrLn, stderr)

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           Data.IORef
import           Foreign (unsafePerformIO)

import           Text.Format (formatS)
import           Util (stripVersion)

data LogLevel = TRACE | DEBUG | INFO | WARN | ERROR
  deriving (Eq, Show)

instance Lift LogLevel where
  lift TRACE = [|TRACE|]
  lift DEBUG = [|DEBUG|]
  lift INFO  = [|INFO|]
  lift WARN  = [|WARN|]
  lift ERROR = [|ERROR|]

data LogRecord = LogRecord {
  logChannel      :: String
, logLevel        :: LogLevel
, logMessage      :: ShowS
, logLocationInfo :: String
}

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
defaultLogSink (LogRecord _ level message linfo) =
  hPutStrLn stderr $ show level ++ " " ++ linfo ++ ": " ++ message []

createLogRecord :: LogLevel -> String -> Q Exp
createLogRecord level message = do
  loc <- location
  let channel = (stripVersion $ loc_package loc) ++ "." ++ loc_module loc
  let filename = loc_filename loc
  let (line, _) = loc_start loc
  let linfo = filename ++ ":" ++ show line
  [| LogRecord channel level $(formatS message) linfo |]

logTrace, logDebug, logInfo, logWarn, logError :: String -> ExpQ
logTrace message = [| consumeLogRecord $(createLogRecord TRACE message) |]
logDebug message = [| consumeLogRecord $(createLogRecord DEBUG message) |]
logInfo  message = [| consumeLogRecord $(createLogRecord INFO  message) |]
logWarn  message = [| consumeLogRecord $(createLogRecord WARN  message) |]
logError message = [| consumeLogRecord $(createLogRecord ERROR message) |]

emitError :: LogRecord -> a
emitError m = Prelude.error $ logMessage m (" (" ++ logLocationInfo m ++ ")")

error :: String -> Q Exp
error message = [|emitError $(createLogRecord ERROR message)|]

undefined :: Q Exp
undefined = [|$(error "undefined")|]

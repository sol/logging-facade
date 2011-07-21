{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Logging (logTrace, logDebug, logInfo, logWarn, logError, LogRecord(..), LogLevel(..), setLogSink, defaultLogSink) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Data.IORef
import           Foreign (unsafePerformIO)

import           DynamicSpec (format)

data LogLevel = TRACE | DEBUG | INFO | WARN | ERROR
  deriving Show

instance Lift LogLevel where
  lift TRACE = [|TRACE|]
  lift DEBUG = [|DEBUG|]
  lift INFO  = [|INFO|]
  lift WARN  = [|WARN|]
  lift ERROR = [|ERROR|]

data LogRecord = LogRecord {
  logLevel        :: LogLevel
, logMessage      :: [Text]
, logLocationInfo :: Text
} deriving Show

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

-- | Write log messages to stdout.
defaultLogSink :: LogRecord -> IO ()
defaultLogSink (LogRecord level message linfo) =
  Text.putStrLn $ Text.concat $ Text.pack (show level) : " " : linfo : ": " : message

createLogRecord :: LogLevel -> String -> Q Exp
createLogRecord level message = do
  loc <- location
  let filename = loc_filename loc
  let (line, _) = loc_start loc
  let linfo = filename ++ ':' : show line
  [| LogRecord level $(format message) (Text.pack linfo) |]

logTrace, logDebug, logInfo, logWarn, logError :: String -> ExpQ
logTrace message = [| consumeLogRecord $(createLogRecord TRACE message) |]
logDebug message = [| consumeLogRecord $(createLogRecord DEBUG message) |]
logInfo  message = [| consumeLogRecord $(createLogRecord INFO  message) |]
logWarn  message = [| consumeLogRecord $(createLogRecord WARN  message) |]
logError message = [| consumeLogRecord $(createLogRecord ERROR message) |]

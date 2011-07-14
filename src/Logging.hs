{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Logging where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

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

createLogRecord :: LogLevel -> String -> Q Exp
createLogRecord level message = do
  loc <- location
  let filename = loc_filename loc
  let (line, _) = loc_start loc
  let linfo = filename ++ ':' : show line
  [| LogRecord level $(format message) (Text.pack linfo) |]

writeLogRecord :: LogRecord -> IO ()
writeLogRecord (LogRecord level message linfo) =
  Text.putStrLn $ Text.concat $ Text.pack (show level) : " " : linfo : ": " : message

logTrace, logDebug, logInfo, logWarn, logError :: String -> ExpQ
logTrace message = [| writeLogRecord $(createLogRecord TRACE message) |]
logDebug message = [| writeLogRecord $(createLogRecord DEBUG message) |]
logInfo  message = [| writeLogRecord $(createLogRecord INFO  message) |]
logWarn  message = [| writeLogRecord $(createLogRecord WARN  message) |]
logError message = [| writeLogRecord $(createLogRecord ERROR message) |]

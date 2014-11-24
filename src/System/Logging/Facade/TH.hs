{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- This module is intended to be imported qualified:
--
-- > import qualified System.Logging.Facade.TH as Log
module System.Logging.Facade.TH (
-- * Producing log messages
  log
, trace
, debug
, info
, warn
, error

-- * Types
, Logging
, LogLevel(..)
) where

import           Prelude hiding (mod, log, error)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           System.Logging.Facade.Types
import           System.Logging.Facade.Class

-- |
-- A Template Haskell version of `System.Logging.Facade.log` that adds a
-- source location to the produced log record.
log :: ExpQ
log = [|\level -> consumeLogRecord . LogRecord level $(mkLocation)|]

-- |
-- A Template Haskell version of `System.Logging.Facade.trace` that adds a
-- source location to the produced log record.
trace :: ExpQ
trace = [|$(log) TRACE|]

-- |
-- A Template Haskell version of `System.Logging.Facade.debug` that adds a
-- source location to the produced log record.
debug :: ExpQ
debug = [|$(log) DEBUG|]

-- |
-- A Template Haskell version of `System.Logging.Facade.info` that adds a
-- source location to the produced log record.
info :: ExpQ
info = [|$(log) INFO|]

-- |
-- A Template Haskell version of `System.Logging.Facade.warn` that adds a
-- source location to the produced log record.
warn :: ExpQ
warn = [|$(log) WARN|]

-- |
-- A Template Haskell version of `System.Logging.Facade.error` that adds a
-- source location to the produced log record.
error :: ExpQ
error = [|$(log) ERROR|]

instance Lift LogLevel where
  lift level = case level of
    TRACE -> [|TRACE|]
    DEBUG -> [|DEBUG|]
    INFO -> [|INFO|]
    WARN -> [|WARN|]
    ERROR -> [|ERROR|]

mkLocation :: ExpQ
mkLocation = do
  loc <- location
  let file = loc_filename loc
      package = loc_package loc
      mod = loc_module loc
      (line, column) = loc_start loc
  [|Just (Location {locationPackage = package, locationModule = mod, locationFile = file, locationLine = line, locationColumn = column})|]

{-# LANGUAGE TemplateHaskell #-}
-- |
-- This module is intended to be imported qualified:
--
-- > import qualified System.Logging.Facade.TH as Log
module System.Logging.Facade.TH (
  Logging
, trace
, debug
, info
, warn
, error
) where

import           Prelude hiding (mod, log, error)
import           Language.Haskell.TH

import           System.Logging.Facade.Types
import           System.Logging.Facade.Class

trace :: ExpQ
trace = [|log TRACE (Just $(mkLocation))|]

debug :: ExpQ
debug = [|log DEBUG (Just $(mkLocation))|]

info :: ExpQ
info = [|log INFO (Just $(mkLocation))|]

warn :: ExpQ
warn = [|log WARN (Just $(mkLocation))|]

error :: ExpQ
error = [|log ERROR (Just $(mkLocation))|]

mkLocation :: ExpQ
mkLocation = do
  loc <- location
  let file = loc_filename loc
      package = loc_package loc
      mod = loc_module loc
      (line, column) = loc_start loc
  [|Location {locationFile = file, locationPackage = package, locationModule =  mod, locationLine = line, locationColumn = column}|]

-- |
-- This module is intended to be imported qualified:
--
-- > import qualified System.Logging.Facade as Log
module System.Logging.Facade (
  Logging
, trace
, debug
, info
, warn
, error
) where

import           Prelude hiding (log, error)

import           System.Logging.Facade.Types
import           System.Logging.Facade.Class

trace :: Logging m => String -> m ()
trace = log TRACE Nothing

debug :: Logging m => String -> m ()
debug = log DEBUG Nothing

info :: Logging m => String -> m ()
info = log INFO Nothing

warn :: Logging m => String -> m ()
warn = log WARN Nothing

error :: Logging m => String -> m ()
error = log ERROR Nothing

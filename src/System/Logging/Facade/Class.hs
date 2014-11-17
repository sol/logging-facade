module System.Logging.Facade.Class where

import           System.Logging.Facade.Types
import           System.Logging.Facade.Sink

class Monad m => Logging m where
  log :: LogLevel -> Maybe Location -> String -> m ()

instance Logging IO where
  log level location message = do
    sink <- getLogSink
    sink level location message

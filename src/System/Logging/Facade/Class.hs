module System.Logging.Facade.Class where

import           System.Logging.Facade.Types
import           System.Logging.Facade.Sink

-- | A type class for monads with logging support
class Monad m => Logging m where
  consumeLogRecord :: LogRecord -> m ()

-- | Log messages that are produced in the `IO` monad are consumed by the
-- global `LogSink`.
instance Logging IO where
  consumeLogRecord record = do
    sink <- getLogSink
    sink record

{-# LANGUAGE CPP #-}
module System.Logging.Facade.Class where

import           Data.Monoid
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Error
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.List
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import           Control.Monad.Trans.State.Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import           Control.Monad.Trans.Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

#if MIN_VERSION_transformers(0,4,0)
import           Control.Monad.Trans.Except
#endif

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

instance (Logging m) => Logging (ContT r m) where
  consumeLogRecord = lift . consumeLogRecord

instance (Error e, Logging m) => Logging (ErrorT e m) where
  consumeLogRecord = lift . consumeLogRecord

instance (Logging m) => Logging (IdentityT m) where
  consumeLogRecord = lift . consumeLogRecord

instance (Logging m) => Logging (ListT m) where
  consumeLogRecord = lift . consumeLogRecord

instance (Logging m) => Logging (MaybeT m) where
  consumeLogRecord = lift . consumeLogRecord

instance (Logging m) => Logging (ReaderT r m) where
  consumeLogRecord = lift . consumeLogRecord

instance (Monoid w, Logging m) => Logging (RWST r w s m) where
  consumeLogRecord = lift . consumeLogRecord

instance (Monoid w, Logging m) => Logging (Strict.RWST r w s m) where
  consumeLogRecord = lift . consumeLogRecord

instance (Logging m) => Logging (StateT s m) where
  consumeLogRecord = lift . consumeLogRecord

instance (Logging m) => Logging (Strict.StateT s m) where
  consumeLogRecord = lift . consumeLogRecord

instance (Monoid w, Logging m) => Logging (WriterT w m) where
  consumeLogRecord = lift . consumeLogRecord

instance (Monoid w, Logging m) => Logging (Strict.WriterT w m) where
  consumeLogRecord = lift . consumeLogRecord

#if MIN_VERSION_transformers(0,4,0)
instance (Logging m) => Logging (ExceptT e m) where
  consumeLogRecord = lift . consumeLogRecord
#endif

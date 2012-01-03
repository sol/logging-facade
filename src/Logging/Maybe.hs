{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Logging.Maybe where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer

import Logging.Type

instance (MonadLogging m) => MonadLogging (MaybeT m) where
  consumeLogRecord = lift . consumeLogRecord

newtype Logging a = Logging {runPure :: Writer [LogRecord] a}
  deriving (Functor, Monad)

instance MonadLogging Logging where
  consumeLogRecord = Logging . tell . return

withLogging :: Logging a -> (a, [LogRecord])
withLogging (Logging a) = runWriter a

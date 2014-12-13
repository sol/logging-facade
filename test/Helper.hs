module Helper (
  module Test.Hspec
, logSinkSpy
) where

import           Test.Hspec
import           Data.IORef

import           System.Logging.Facade.Types
import           System.Logging.Facade.Sink

logSinkSpy :: IO (IO [LogRecord], LogSink)
logSinkSpy = do
  ref <- newIORef []
  let spy :: LogSink
      spy record = modifyIORef ref (record {logRecordLocation = Nothing} :)
  return (readIORef ref, spy)

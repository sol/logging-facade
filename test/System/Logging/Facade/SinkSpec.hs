module System.Logging.Facade.SinkSpec (main, spec) where

import           Helper

import           System.Logging.Facade
import           System.Logging.Facade.Types
import           System.Logging.Facade.Sink

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "withLogSink" $ do
    it "sets the global log sink to specified value before running specified action" $ do
      (logRecords, spy) <- logSinkSpy
      withLogSink spy (info "some log message")
      logRecords `shouldReturn` [LogRecord INFO Nothing "some log message"]

    it "restores the original log sink when done" $ do
      (logRecords, spy) <- logSinkSpy
      setLogSink spy
      withLogSink (\_ -> return ()) (return ())
      info "some log message"
      logRecords `shouldReturn` [LogRecord INFO Nothing "some log message"]

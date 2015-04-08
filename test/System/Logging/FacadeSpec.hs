module System.Logging.FacadeSpec (main, spec) where

import           Test.Hspec
import           Data.IORef

import           System.Logging.Facade.Types
import           System.Logging.Facade.Sink
import           System.Logging.Facade as Log

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let logToIORef :: IORef [LogRecord] -> LogSink
      logToIORef ref record = modifyIORef ref (record :)

  describe "info" $ do
    it "writes a log message with log level INFO" $ do
      ref <- newIORef []
      setLogSink $ logToIORef ref
      info "some log message"
      readIORef ref `shouldReturn` [LogRecord INFO Nothing "some log message"]

  describe "captureLogs" $ do
    it "returns all logs of an action" $ do
      (logs, ()) <- captureLogs $ do
        Log.trace "this should be captured"
        Log.trace "this should be captured next"
      logs `shouldBe` [ LogRecord TRACE Nothing "this should be captured"
                      , LogRecord TRACE Nothing "this should be captured next"
                      ]

    it "prevents logs from going to the log sink" $ do
      ref <- newIORef []
      setLogSink $ logToIORef ref
      _ <- captureLogs $ Log.trace "this should be captured"
      readIORef ref `shouldReturn` []

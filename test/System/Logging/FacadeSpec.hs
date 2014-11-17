module System.Logging.FacadeSpec (main, spec) where

import           Test.Hspec
import           Data.IORef

import           System.Logging.Facade.Types
import           System.Logging.Facade.Sink
import           System.Logging.Facade

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "info" $ do
    it "writes a log message with log level INFO" $ do
      ref <- newIORef []
      let captureLogMessage :: LogSink
          captureLogMessage level location message = modifyIORef ref ((level, location, message) :)
      setLogSink captureLogMessage
      info "some log message"
      readIORef ref `shouldReturn` [(INFO, Nothing, "some log message")]

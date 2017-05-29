{-# LANGUAGE CPP #-}
module System.Logging.Facade.Sink (
  LogSink
, defaultLogSink
, getLogSink
, setLogSink
, swapLogSink
, withLogSink
) where

import           Control.Concurrent
import           Data.IORef
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           Control.Exception

import           System.Logging.Facade.Types

-- | A consumer for log records
type LogSink = LogRecord -> IO ()

-- use the unsafePerformIO hack to share one sink across a process
logSink :: IORef LogSink
{-# NOINLINE logSink #-}
logSink = unsafePerformIO action
  where
    action = do
      hSetBuffering stderr LineBuffering
      defaultLogSink >>= newIORef

-- | Return the global log sink.
getLogSink :: IO LogSink
getLogSink = readIORef logSink

-- | Set the global log sink.
setLogSink :: LogSink -> IO ()
setLogSink = atomicWriteIORef logSink

-- | Return the global log sink and set it to a new value in one atomic
-- operation.
swapLogSink :: LogSink -> IO LogSink
swapLogSink new = atomicModifyIORef logSink $ \old -> (new, old)

-- | Set the global log sink to a specified value, run given action, and
-- finally restore the global log sink to its previous value.
withLogSink :: LogSink -> IO () -> IO ()
withLogSink sink action = bracket (swapLogSink sink) setLogSink (const action)

-- | A thread-safe log sink that writes log messages to `stderr`
defaultLogSink :: IO LogSink
defaultLogSink = defaultLogSink_ `fmap` newMVar ()

defaultLogSink_ :: MVar () -> LogSink
defaultLogSink_ mvar record = withMVar mvar (\() -> hPutStrLn stderr output)
  where
    level = logRecordLevel record
    mLocation = logRecordLocation record
    message = logRecordMessage record
    output = shows level . location . showString ": " . showString message $ ""
    location = maybe (showString "") ((showString " " .) . formatLocation) mLocation

formatLocation :: Location -> ShowS
formatLocation loc = showString (locationFile loc) . colon . shows (locationLine loc) . colon . shows (locationColumn loc)
  where colon = showString ":"

#if !MIN_VERSION_base(4,6,0)
atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef ref a = do
    x <- atomicModifyIORef ref (\_ -> (a, ()))
    x `seq` return ()
#endif

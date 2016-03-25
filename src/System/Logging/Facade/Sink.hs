module System.Logging.Facade.Sink (
  LogSink
, defaultLogSink
, setLogSink
, getLogSink
) where

import           Control.Concurrent
import           Data.IORef
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)

import           System.Logging.Facade.Types

-- | A consumer for log records
type LogSink = LogRecord -> IO ()

-- use the unsafePerformIO hack to share one sink across a process
logSink :: IORef LogSink
logSink = unsafePerformIO (defaultLogSink >>= newIORef)
{-# NOINLINE logSink #-}

-- | Return the global log sink.
getLogSink :: IO LogSink
getLogSink = readIORef logSink

-- | Set the global log sink.
setLogSink :: LogSink -> IO ()
setLogSink = atomicWriteIORef logSink

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

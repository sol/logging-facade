module System.Logging.Facade.Sink (
  LogSink
, defaultLogSink
, setLogSink
, getLogSink
) where

import           Data.IORef
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)

import           System.Logging.Facade.Types

-- | A consumer for log records
type LogSink = LogRecord -> IO ()

-- use the unsafePerformIO hack to share one sink across a process
logSink :: IORef LogSink
logSink = unsafePerformIO (newIORef defaultLogSink)
{-# NOINLINE logSink #-}

-- | Return the global log sink.
getLogSink :: IO LogSink
getLogSink = readIORef logSink

-- | Set the global log sink.
setLogSink :: LogSink -> IO ()
setLogSink = atomicWriteIORef logSink

-- | A log sink that writes log messages to `stderr`
defaultLogSink :: LogSink
defaultLogSink record = hPutStrLn stderr output
  where
    level = logRecordLevel record
    mLocation = logRecordLocation record
    message = logRecordMessage record
    output = shows level . location . showString ": " . showString message $ ""
    location = maybe (showString "") ((showString " " .) . formatLocation) mLocation

formatLocation :: Location -> ShowS
formatLocation loc = showString (locationFile loc) . colon . shows (locationLine loc) . colon . shows (locationColumn loc)
  where colon = showString ":"

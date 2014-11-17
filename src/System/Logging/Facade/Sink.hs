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

type LogSink = LogLevel -> Maybe Location -> String -> IO ()

-- use the unsafePerformIO hack to share one sink across a process
logSink :: IORef LogSink
logSink = unsafePerformIO (newIORef defaultLogSink)
{-# NOINLINE logSink #-}

-- | Return the global `LogSink`.
getLogSink :: IO LogSink
getLogSink = readIORef logSink

-- | Set the global `LogSink`.
setLogSink :: LogSink -> IO ()
setLogSink = atomicWriteIORef logSink

-- | Write log messages to stderr.
defaultLogSink :: LogSink
defaultLogSink level mLocation message = hPutStrLn stderr output
  where
    output = shows level . location . showString ": " . showString message $ ""
    location = maybe (showString "") ((showString " " .) . formatLocation) mLocation

formatLocation :: Location -> ShowS
formatLocation loc = showString (locationFile loc) . colon . shows (locationLine loc) . colon . shows (locationColumn loc)
  where colon = showString ":"

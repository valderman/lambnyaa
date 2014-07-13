module Network.LambNyaa.Log.Core (
    LogItem, Logger, LogLevel (..),
    MonadLog (..),
    liSource, liLevel, liTags, liTimestamp, liMessage,
    newLogItem, unsafeNewLogItem,
    debug, info, warn, err,
    logToStdout, logToStderr, formatLogItem
  ) where
import Data.Time
import Data.List
import System.IO.Unsafe
import System.Locale
import System.IO

-- | Urgency of a log item.
data LogLevel = Debug | Info | Warning | Error
  deriving (Show, Read, Eq, Ord)

data LogItem = LogItem {
    liSource    :: String,
    liLevel     :: LogLevel,
    liTags      :: [String],
    liTimestamp :: UTCTime,
    liMessage   :: String
  }

-- | Any monad capable of keeping a log. Minimal definition: logMessage'.
class Monad m => MonadLog m where
  -- | Log a message using the default source for the monad, and no tags.
  logMessage  :: LogLevel -> String -> m ()
  logMessage = logWithTags []

  -- | Log a message using a set of tags. This is primarily useful to pass
  --   extra information about a log item to a custom logger.
  logWithTags :: [String] -> LogLevel -> String -> m ()
  logWithTags = logMessage' ""

  -- | Log a message with a custom source and tags.
  logMessage' :: String -> [String] -> LogLevel -> String -> m ()

debug, info, warn, err :: MonadLog m => String -> m ()
debug = logMessage Debug
info  = logMessage Info
warn  = logMessage Warning
err   = logMessage Error

-- | Create a new timestamped log item in the IO monad.
newLogItem :: LogLevel -> String -> [String] -> String -> IO LogItem
newLogItem level src tags msg = do
  now <- getCurrentTime
  return $ LogItem {
    liSource    = src,
    liLevel     = level,
    liTags      = tags,
    liTimestamp = now,
    liMessage   = msg
  }

-- | Create a new timestamped log item outside the IO monad.
--   Don't use this unless absolutely necessary, as log item's time stamp will
--   reflect the point in time when it was first evaluated rather than the time
--   of the call.
{-# NOINLINE unsafeNewLogItem #-}
unsafeNewLogItem :: LogLevel -> String -> [String] -> String -> LogItem
unsafeNewLogItem level src tags msg =
  unsafePerformIO $ newLogItem level src tags msg

type Logger = LogItem -> IO ()

-- | Format a log item as (time) [level] [source] [tag1, .. tagN] message
formatLogItem :: LogItem -> String
formatLogItem li =
  concat $ [formatTime defaultTimeLocale "(%c) " $ liTimestamp li,
            concat ["[", show $ liLevel li, "] "],
            if null $ liSource li
              then ""
              else concat ["[", liSource li, "] "],
            if null $ liTags li
              then ""
              else concat ["[",intercalate ", " $ liTags li,"] "],
            liMessage li]

-- | Print log to stdout.
logToStdout :: Logger
logToStdout = hPutStrLn stdout . formatLogItem

-- | Print log to stderr.
logToStderr :: Logger
logToStderr = hPutStrLn stderr . formatLogItem

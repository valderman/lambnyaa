{-# LANGUAGE ExistentialQuantification #-}
module Network.LambNyaa.Log.Core (
    LogItem, LogHandler (..), LogLevel (..),
    MonadLog (..), logMessage,
    liSource, liLevel, liTags, liTimestamp, liMessage,
    newLogItem, unsafeNewLogItem, formatLogItem,
    debug, info, warn, err,
    logToStdout, logToStderr, logToFile, logToNewFile
  ) where
import Data.Time
import Data.List
import System.IO.Unsafe
import System.Locale
import System.IO
import Control.Concurrent

-- | Urgency of a log item.
data LogLevel = Debug | Info | Warning | Error
  deriving (Show, Read, Eq, Ord)

-- | A log handler is an IO action which performs any initialization needed
--   for the handler to work, and returns a pair of a logger function and a
--   cleanup function. See @logToFile@ for more a usage example.
type LogHandler = IO (LogItem -> IO (), IO ())

-- | A log item. Contains a log message and assorted information about its
--   origins.
data LogItem = LogItem {
    liSource    :: String,   -- ^ The source of the log item, as given to
                             --   @logMessage@.
    liLevel     :: LogLevel, -- ^ Urgency of the log item.
    liTags      :: [String], -- ^ Any tags associated by the log item.
    liTimestamp :: UTCTime,  -- ^ Time when message was logged.
    liMessage   :: String    -- ^ The message itself.
  }

-- | Any monad capable of keeping a log.
class Monad m => MonadLog m where
  -- | Log a message using a list of tags. This is primarily useful to pass
  --   extra information about a log item to a custom logger.
  logWithTags :: [String] -> LogLevel -> String -> String -> m ()

-- | Log a message without any tags.
logMessage :: MonadLog m => LogLevel -> String -> String -> m ()
logMessage = logWithTags []

debug, info, warn, err :: MonadLog m => String -> String -> m ()
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
logToStdout :: LogHandler
logToStdout = return (hPutStrLn stdout . formatLogItem, hFlush stdout)

-- | Print log to stderr.
logToStderr :: LogHandler
logToStderr = return (hPutStrLn stderr . formatLogItem, hFlush stderr)

fileLogger :: IOMode -> FilePath -> LogHandler
fileLogger mode file = do
  h <- openFile file mode
  t <- forkIO $ forever (threadDelay 60000000 >> hFlush h)
  return (hPutStrLn h . formatLogItem, killThread t >> hClose h)

-- | Print log to a file. The log buffer is flushed once every minute.
logToFile :: FilePath -> LogHandler
logToFile = fileLogger AppendMode

-- | Like @logToFile@, but overwrites the file's contents rather than appending
--   to it.
logToNewFile :: FilePath -> LogHandler
logToNewFile = fileLogger WriteMode

forever :: IO () -> IO ()
forever m = m >> forever m

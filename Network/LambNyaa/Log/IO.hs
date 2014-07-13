-- | IO MonadLogger instance and associated utilities.
--   Note that this module only pertains to IO-based loggers.
--   addLogger, removeLogger, etc. will not affect, for instance, Writer-based
--   MonadLog instances.
--   By default, the log level is set to Info.
module Network.LambNyaa.Log.IO (
    LoggerHandle,
    addLogger, removeLogger, clearLoggers, setLoggers, getLogLevel, setLogLevel
  ) where
import Network.LambNyaa.Log.Core
import System.IO.Unsafe
import Control.Concurrent
import Control.Monad
import Data.IORef

-- | Unique handle to a registered logger.
newtype LoggerHandle = LoggerHandle Int deriving (Eq)

{-# NOINLINE logChan #-}
logChan :: Chan LogItem
logChan = unsafePerformIO $ newChan

{-# NOINLINE handles #-}
handles :: IORef Int
handles = unsafePerformIO $ newIORef 0

{-# NOINLINE level #-}
level :: IORef LogLevel
level = unsafePerformIO $ newIORef Info

{-# NOINLINE loggers #-}
loggers :: IORef [(LoggerHandle, Logger)]
loggers = unsafePerformIO $ do
  forkIO $ do
    is <- getChanContents logChan
    forM_ is $ \i -> do
      ll <- getLogLevel
      when (liLevel i >= ll) $ do
        readIORef loggers >>= mapM_ (\(_, h) -> h i)
  newIORef []

instance MonadLog IO where
  logMessage' src tags lvl msg = do
    li <- newLogItem lvl src tags msg
    readIORef loggers >>= mapM_ (\(_, h) -> h li)

newHandle :: IO LoggerHandle
newHandle = atomicModifyIORef' handles (\n -> (n+1, LoggerHandle n))

-- | Register an IO logger.
addLogger :: Logger -> IO LoggerHandle
addLogger logger = do
  h <- newHandle
  atomicModifyIORef' loggers (\ls -> ((h, logger):ls, h))

-- | Unregister an IO logger. Unregistering a non-registered logger is a
--   no-op.
removeLogger :: LoggerHandle -> IO ()
removeLogger h = atomicModifyIORef' loggers (\ls -> (delItem h ls, ()))

delItem :: Eq a => a -> [(a, b)] -> [(a, b)]
delItem i = go
  where
    go (x@(k,_):xs)
      | k == i    = xs
      | otherwise = x:xs
    go _          = []

-- | Unregister all IO loggers.
clearLoggers :: IO ()
clearLoggers = writeIORef loggers []

-- | Unregister all previously registered loggers and replace them with a new
--   list.
setLoggers :: [Logger] -> IO [LoggerHandle]
setLoggers ls = do
  clearLoggers
  mapM addLogger ls

-- | Set the logging level for all IO loggers.
setLogLevel :: LogLevel -> IO ()
setLogLevel = writeIORef level

-- | Inspect the logging level for all IO loggers.
getLogLevel :: IO LogLevel
getLogLevel = readIORef level

-- | IO MonadLogger instance and associated utilities.
--   Note that this module only pertains to IO-based loggers.
--   addLogHandler, etc. will not affect, for instance, Writer-based
--   MonadLog instances.
--   By default, the log level is set to Info.
module Network.LambNyaa.Log.IO (
    LogHandlerName,
    addLogHandler, removeLogHandler,
    clearLogHandlers, setLogHandlers,
    getLogLevel, setLogLevel
  ) where
import Network.LambNyaa.Log.Core
import System.IO.Unsafe
import Control.Concurrent
import Control.Monad
import Data.IORef

-- | Replacement for atomicModifyIORef' since it doesn't exist with GHC < 7.6.
modifyRef :: IORef a -> (a -> (a, b)) -> IO b
modifyRef r f = do
  x <- atomicModifyIORef r $ \x -> let (a, b) = f x in (a, a `seq` b)
  x `seq` return x

-- | Unique handle to a registered log handler.
newtype LogHandlerName = LogHandlerName Int deriving (Eq)

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
loggers :: IORef [(LogHandlerName, (LogItem -> IO (), IO ()))]
loggers = unsafePerformIO $ do
  forkIO $ do
    is <- getChanContents logChan
    forM_ is $ \i -> do
      ll <- getLogLevel
      when (liLevel i >= ll) $ do
        readIORef loggers >>= mapM_ (\(_, h) -> fst h i)
  newIORef []

instance MonadLog IO where
  logWithTags tags lvl src msg = do
    newLogItem lvl src tags msg >>= writeChan logChan

newName :: IO LogHandlerName
newName = modifyRef handles (\n -> (n+1, LogHandlerName n))

-- | Register an IO log handler.
addLogHandler :: LogHandler -> IO LogHandlerName
addLogHandler initHandler = do
  n <- newName
  (logF, closeF) <- initHandler
  modifyRef loggers (\ls -> ((n, (logF, closeF)):ls, n))

-- | Unregister an IO logger. Unregistering a non-registered logger is a
--   no-op.
removeLogHandler :: LogHandlerName -> IO ()
removeLogHandler h = do
  ml <- modifyRef loggers (\ls -> (delItem h ls, lookup h ls))
  maybe (return ()) snd ml

delItem :: Eq a => a -> [(a, b)] -> [(a, b)]
delItem i = go
  where
    go (x@(k,_):xs)
      | k == i    = xs
      | otherwise = x:xs
    go _          = []

-- | Unregister all IO loggers.
clearLogHandlers :: IO ()
clearLogHandlers = do
  ls <- modifyRef loggers (\ls -> ([], ls))
  mapM_ (snd . snd) ls

-- | Unregister all previously registered loggers and replace them with a new
--   list.
setLogHandlers :: [LogHandler] -> IO [LogHandlerName]
setLogHandlers ls = do
  clearLogHandlers
  mapM addLogHandler ls

-- | Set the logging level for all IO loggers.
setLogLevel :: LogLevel -> IO ()
setLogLevel = writeIORef level

-- | Inspect the logging level for all IO loggers.
getLogLevel :: IO LogLevel
getLogLevel = readIORef level

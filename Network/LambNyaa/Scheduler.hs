{-# LANGUAGE PatternGuards #-}
-- | Main executive for LambNyaa.
module Network.LambNyaa.Scheduler (schedule) where
import Control.Concurrent
import Control.Monad
import Data.Hashable
import Data.List
import qualified Database.SQLite.Simple as DB
import Network.LambNyaa.Config
import Network.LambNyaa.Item
import Network.LambNyaa.Monad
import Network.LambNyaa.Sink
import Network.LambNyaa.Database
import Network.LambNyaa.Log
import System.Posix.Signals
import System.Exit

info' = info "Scheduler"
note' = note "Scheduler"

-- | Pause thread for a number of seconds.
delaySecs :: Int -> IO ()
delaySecs n = do
    forM_ [1..ksecs] . const $ threadDelay (1000 * 1000000)
    threadDelay (secs * 1000000)
  where
    (ksecs, secs) = n `divMod` 1000

-- | Perform a clean exit on SIGTERM or SIGINT.
die :: ThreadId -> IO ()
die mainthread = do
  info' $ "Interrupted; performing cleanup and exiting..."
  delaySecs 2
  clearLogHandlers
  throwTo mainthread ExitSuccess

-- | Execute a pipeline according to schedule.
schedule :: Config -> Nyaa () -> IO ()
schedule cfg ny = do
  setLogHandlers $ cfgLogHandlers cfg
  setLogLevel $ cfgLogLevel cfg
  tid <- myThreadId
  when (cfgCatchSignals cfg) $ do
    installHandler keyboardSignal (Catch $ die tid) (Just fullSignalSet)
    installHandler softwareTermination (Catch $ die tid) (Just fullSignalSet)
    return ()
  case cfgSchedule cfg of
    Once              -> do
      info' "Starting oneshot run..."
      tot <- execute cfg ny
      info' $ "Run completed! " ++ show tot ++ " items accepted."
    (Every n Seconds) -> every n (show n ++ " seconds") $ execute cfg ny
    (Every n Minutes) -> every (n*60) (show n ++ " minutes") $ execute cfg ny
    (Every n Hours)   -> every (n*60*60) (show n ++ " hours") $ execute cfg ny
    (Every n Days)    -> every (n*24*60*60) (show n ++ " days") $ execute cfg ny

-- | Perform an action every n seconds.
every :: Int -> String -> IO Int -> IO ()
every secs sched act = do
    note' $ "Starting scheduling; runs are scheduled for every " ++ sched
    go
  where
    go = do
      info' "Starting new run..."
      tot <- act
      info' $ "Run completed! " ++ show tot ++ " items accepted."
      delaySecs secs >> go

-- | Execute a pipeline from Source to Sink, and return the total number of
--   elements processed as well as the total number accepted.
execute :: Config -> Nyaa () -> IO Int
execute cfg nyaa = withSQLite cfg $ \conn -> runNyaa (NyaaEnv conn cfg) nyaa

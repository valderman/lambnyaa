{-# LANGUAGE PatternGuards #-}
-- | Main executive for LambNyaa.
module Network.LambNyaa.Scheduler (schedule) where
import Control.Concurrent
import Control.Monad
import Data.Hashable
import Data.List
import qualified Database.SQLite.Simple as DB
import Network.LambNyaa.Config
import Network.LambNyaa.Types
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
schedule :: Config -> IO ()
schedule cfg = do
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
      (t, a) <- execute cfg
      info' $ "Run completed! " ++ show a ++ "/" ++ show t ++ " items accepted."
    (Every n Seconds) -> every n (show n ++ " seconds") $ execute cfg
    (Every n Minutes) -> every (n*60) (show n ++ " minutes") $ execute cfg
    (Every n Hours)   -> every (n*60*60) (show n ++ " hours") $ execute cfg
    (Every n Days)    -> every (n*24*60*60) (show n ++ " days") $ execute cfg

-- | Perform an action every n seconds.
every :: Int -> String -> IO (Int, Int) -> IO ()
every secs sched act = do
    note' $ "Starting scheduling; runs are scheduled for every " ++ sched
    go
  where
    go = do
      info' "Starting new run..."
      (t, a) <- act
      info' $ "Run completed! " ++ show a ++ "/" ++ show t ++ " items accepted."
      delaySecs secs >> go

-- | Execute a pipeline from Source to Sink, and return the total number of
--   elements processed as well as the total number accepted.
execute :: Config -> IO (Int, Int)
execute cfg = do
  -- Create Items from sources
  itemses <- mapM unSource $ cfgSources cfg
  -- Fill in identifiers
  let items = concat $ map (map (\i -> i {itmIdentifier = hash i})) itemses
  -- Fill in seen before info
  items' <- withSQLite cfg $ \c -> mapM (fillSeen c) items
  -- Run pipeline
  accepted <- fillSinks cfg [(sink, item) |
                             flt <- cfgFilters cfg,
                             Accept sinks item <- map flt items',
                             sink <- sinks]
  return (length items, accepted)

-- | Fill in the "seen before" field of an Item.
fillSeen :: DB.Connection -> Item -> IO Item
fillSeen c item = do
  seen <- wasSeen (itmIdentifier item) c
  return $ item {itmSeenBefore = seen}

-- | Batch all Items bound for a particular Sink, then shove that batch into
--   said Sink. Repeat for all distinct Sinks in list and return the number of
--   total accepted items.
fillSinks :: Config -> [(Sink, Item)] -> IO Int
fillSinks cfg items = do
    mapM_ feedSinks
      . groupBy (\(a, _) (b, _) -> a == b)
      . sortBy (\(a, _) (b, _) -> compare a b)
      $ items
    return $ length items
  where
    feedSinks :: [(Sink, Item)] -> IO ()
    feedSinks xs = sinkHandler (fst $ head xs) cfg (map snd xs)

{-# LANGUAGE PatternGuards #-}
-- | Main executive for LambNyaa.
module Network.LambNyaa.Scheduler (schedule) where
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Hashable
import Data.List
import qualified Database.SQLite.Simple as DB
import Network.LambNyaa.Config
import Network.LambNyaa.Types
import Network.LambNyaa.Sink
import Network.LambNyaa.Database
import Network.LambNyaa.Log

-- | Pause thread for a number of seconds.
delaySecs :: Int -> IO ()
delaySecs n = do
    forM_ [1..ksecs] . const $ threadDelay (1000 * 1000000)
    threadDelay (secs * 1000000)
  where
    (ksecs, secs) = n `divMod` 1000

-- | Execute a pipeline according to schedule.
schedule :: Config -> IO ()
schedule cfg = do
  setLoggers [cfgLogger cfg]
  setLogLevel $ cfgLogLevel cfg
  case cfgSchedule cfg of
    Once              -> do
      info "Starting oneshot run..."
      execute cfg
      info "Run completed!"
    (Every n Seconds) -> every n (show n ++ " seconds") $ execute cfg
    (Every n Minutes) -> every (n*60) (show n ++ " minutes") $ execute cfg
    (Every n Hours)   -> every (n*60*60) (show n ++ " hours") $ execute cfg
    (Every n Days)    -> every (n*24*60*60) (show n ++ " days") $ execute cfg

-- | Perform an action every n seconds.
every :: Int -> String -> IO () -> IO ()
every secs sched act = do
    info $ "Starting scheduling; runs are scheduled for every " ++ sched
    go
  where
    go = do
      info "Starting new run..."
      act
      info "Run completed!"
      delaySecs secs >> go

-- | Execute a pipeline, from Source to Sink.
execute :: Config -> IO ()
execute cfg = do
  -- Create Items from sources
  itemses <- mapM unSource $ cfgSources cfg
  -- Fill in identifiers
  let items = concat $ map (map (\i -> i {itmIdentifier = hash i})) itemses
  -- Fill in seen before info
  items' <- withSQLite cfg $ \c -> mapM (fillSeen c) items
  -- Run pipeline
  fillSinks cfg [(sink, item) |
                 flt <- cfgFilters cfg,
                 Accept sinks item <- map flt items',
                 sink <- sinks]

-- | Fill in the "seen before" field of an Item.
fillSeen :: DB.Connection -> Item -> IO Item
fillSeen c item = do
  seen <- wasSeen (itmIdentifier item) c
  return $ item {itmSeenBefore = seen}

-- | Batch all Items bound for a particular Sink, then shove that batch into
--   said Sink. Repeat for all distinct Sinks in list.
fillSinks :: Config -> [(Sink, Item)] -> IO ()
fillSinks cfg =
    mapM_ feedSinks
      . groupBy (\(a, _) (b, _) -> a == b)
      . sortBy (\(a, _) (b, _) -> compare a b)
  where
    feedSinks :: [(Sink, Item)] -> IO ()
    feedSinks xs = sinkHandler (fst $ head xs) cfg (map snd xs)

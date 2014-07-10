{-# LANGUAGE PatternGuards #-}
-- | Main executive for LambNyaa.
module Network.LambNyaa.Scheduler (schedule) where
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Hashable
import Data.List
import Data.IORef
import qualified Database.SQLite.Simple as DB
import Network.LambNyaa.Config
import Network.LambNyaa.Types
import Network.LambNyaa.Sink
import Network.LambNyaa.Database
import Network.LambNyaa.FiniteChan


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
  case cfgSchedule cfg of
    Once              -> execute cfg
    (Every n Seconds) -> every n $ execute cfg
    (Every n Minutes) -> every (n*60) $ execute cfg
    (Every n Hours)   -> every (n*60*60) $ execute cfg
    (Every n Days)    -> every (n*24*60*60) $ execute cfg

-- | Perform an action every n seconds.
every :: Int -> IO () -> IO ()
every secs m = go where go = m >> delaySecs secs >> go

-- | Execute a pipeline, from Source to Sink.
--   TODO:
--     * nub isn't very efficient, replace with something smarter.
--     * with barrier sync, we could run all sinks in parallel
execute :: Config -> IO ()
execute cfg = do
  -- Create Items from sources
  itemses <- mapM unSource $ cfgSources cfg
  -- Fill in identifiers
  let items = concat $ map (map (\i -> i {itmIdentifier = hash i})) itemses
  -- Fill in seen before info
  items' <- withSQLite cfg $ \c -> mapM (fillSeen c) items
  -- Run pipeline
  sinks <- nub `fmap` sequence [act >> return s |
                                flt <- cfgFilters cfg,
                                Accept s act <- map flt items']
  -- Handle items
  mapM_ (\s -> readIORef (sinkChan s) >>= closeChan) sinks
  mapM_ (flip sinkHandler cfg) sinks
  -- Reset sinks
  mapM_ reinitSink sinks

fillSeen :: DB.Connection -> Item -> IO Item
fillSeen c item = do
  seen <- wasSeen (itmIdentifier item) c
  return $ item {itmSeenBefore = seen}

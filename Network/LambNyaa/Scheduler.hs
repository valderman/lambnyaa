{-# LANGUAGE PatternGuards #-}
module Network.LambNyaa.Scheduler (schedule) where
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Network.LambNyaa.Config
import Network.LambNyaa.Types

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
execute :: Config -> IO ()
execute cfg = do
  itemses <- mapM unSource $ cfgSources cfg
  sequence_ $ [act cfg |
               flt <- cfgFilters cfg,
               items <- itemses,
               item <- items,
               Accept act <- [flt item]]

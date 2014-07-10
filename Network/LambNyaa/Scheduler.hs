{-# LANGUAGE PatternGuards #-}
module Network.LambNyaa.Scheduler (schedule) where
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Network.LambNyaa.Config
import Network.LambNyaa.Types

delaySecs :: Int -> IO ()
delaySecs n = do
    forM_ [1..ksecs] . const $ threadDelay (1000 * 1000000)
    threadDelay (secs * 1000000)
  where
    (ksecs, secs) = n `divMod` 1000

schedule :: Config -> IO ()
schedule cfg = do
  case cfgSchedule cfg of
    Once              -> execute cfg
    (Every n Seconds) -> every n $ execute cfg
    (Every n Minutes) -> every (n*60) $ execute cfg
    (Every n Hours)   -> every (n*60*60) $ execute cfg
    (Every n Days)    -> every (n*24*60*60) $ execute cfg

every :: Int -> IO () -> IO ()
every secs m = go where go = m >> delaySecs secs >> go

execute :: Config -> IO ()
execute cfg = do
  itemses <- mapM unSource $ cfgSources cfg
  sequence_ $ [unSink (flt item) item |
               flt <- cfgFilters cfg,
               items <- itemses,
               item <- items]

module Network.LambNyaa.Sink (
    Sink (..), sink, sink_, printItem, seen, unseen
  ) where
import Network.LambNyaa.Types
import Network.LambNyaa.Database
import System.IO.Unsafe
import Data.IORef

{-# NOINLINE sinkCtr #-}
sinkCtr :: IORef Int
sinkCtr = unsafePerformIO $ newIORef 0

-- | Create a Sink from any IO action.
{-# NOINLINE sink #-}
sink :: (Config -> [Item] -> IO ()) -> Sink
sink f = unsafePerformIO $ do
  ctr <- atomicModifyIORef' sinkCtr (\ctr -> (ctr+1, ctr))
  return $ Sink {
      sinkHandler = f,
      sinkID      = ctr
    }

-- | Create a Sink from any IO action; Config-less version.
sink_ :: ([Item] -> IO ()) -> Sink
sink_ f = sink $ const f

-- | Print an Item to stdout.
printItem :: Sink
printItem = sink_ $ mapM_ print

-- | Mark an item as seen.
seen :: Sink
seen = sink $ \cfg is -> withSQLite cfg $ \c -> do
  mapM_ (\i -> markSeen (itmIdentifier i) True c) is

-- | Mark an item as seen.
unseen :: Sink
unseen = sink $ \cfg is -> withSQLite cfg $ \c -> do
  mapM_ (\i -> markSeen (itmIdentifier i) False c) is

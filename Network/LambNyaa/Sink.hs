module Network.LambNyaa.Sink (
    Sink (..), sink, sink_, printItem, seen, unseen
  ) where
import Data.Monoid
import Network.LambNyaa.Config
import Network.LambNyaa.Item
import Network.LambNyaa.Database

newtype Sink = Sink {unSink :: Config -> [Item] -> IO ()}

-- | Create a Sink from any IO action.
sink :: (Config -> [Item] -> IO ()) -> Sink
sink = Sink

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

instance Monoid Sink where
  mempty = sink_ . const $ return ()
  mappend a b = sink $ \cfg is -> do
    unSink a cfg is
    unSink b cfg is

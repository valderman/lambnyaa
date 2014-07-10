module Network.LambNyaa.Sink (
    Sink (..), sink, sink_, printItem, seen, unseen
  ) where
import Data.Monoid
import Network.LambNyaa.Types
import Network.LambNyaa.Config
import Network.LambNyaa.Database

-- | Create a Sink from any IO action.
sink :: (Item -> Config -> IO ()) -> Sink
sink = Sink

-- | Create a Sink from any IO action; Config-less version.
sink_ :: (Item -> IO ()) -> Sink
sink_ f = Sink $ \i _ -> f i

-- | Print an Item to stdout.
printItem :: Sink
printItem = sink_ print

-- | Mark an item as seen.
seen :: Sink
seen = sink $ \i -> markSeen (itmIdentifier i) True

-- | Mark an item as unseen.
unseen :: Sink
unseen = sink $ \i -> markSeen (itmIdentifier i) False

-- | A Sink is the endpoint of a stream. It consists of an IO action taking an
--   Item as its input, which is executed once for each Item that is accepted
--   into the sink.
newtype Sink = Sink {unSink :: (Item -> Config -> IO ())}

instance Monoid Sink where
  mempty = Sink $ \_ _ -> return ()
  mappend (Sink a) (Sink b) = Sink $ \c i -> a c i >> b c i
  mconcat fs = Sink $ \c i -> mapM_ (\(Sink f) -> f c i) fs

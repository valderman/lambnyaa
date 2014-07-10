module Network.LambNyaa.Sink (
    Sink (..), reinitSink, sink, sink_, printItem, seen, unseen
  ) where
import Network.LambNyaa.Types
import Network.LambNyaa.Database
import Network.LambNyaa.FiniteChan
import System.IO.Unsafe
import Data.IORef

-- | Create a Sink from any IO action.
{-# NOINLINE sink #-}
sink :: (Config -> [Item] -> IO ()) -> Sink
sink f = unsafePerformIO $ do
  r <- newChan >>= newIORef
  return $ Sink {
      sinkHandler = \cfg -> readIORef r >>= getChanContents >>= f cfg,
      sinkChan    = r,
      writeSink   = \i -> readIORef r >>= flip writeChan i
    }

-- | Reinitialize a Sink by giving it a new Chan.
reinitSink :: Sink -> IO ()
reinitSink s = newChan >>= writeIORef (sinkChan s)

-- | Create a Sink from any IO action; Config-less version.
sink_ :: ([Item] -> IO ()) -> Sink
sink_ f = sink $ const f

-- | Print an Item to stdout.
printItem :: Sink
printItem = sink_ $ mapM_ print

-- | Mark an item as seen.
seen :: Sink
seen = sink $ \cfg -> mapM_ $ \i -> markSeen (itmIdentifier i) True cfg

-- | Mark an item as unseen.
unseen :: Sink
unseen = sink $ \cfg -> mapM_ $ \i -> markSeen (itmIdentifier i) False cfg

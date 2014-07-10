module Network.LambNyaa.Sink (
    module Download,
    sink, printItem
  ) where
import Data.Monoid
import Network.LambNyaa.Types
import Network.LambNyaa.Sink.Download as Download

-- | Create a Sink from any IO action.
sink :: (Item -> IO ()) -> Sink
sink = Sink

-- | Print an Item to stdout.
printItem :: Sink
printItem = sink print

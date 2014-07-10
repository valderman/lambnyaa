module Network.LambNyaa.Sink (
    sink, printItem
  ) where
import Data.Monoid
import Network.LambNyaa.Types

-- | Create a Sink from any IO action.
sink :: (Item -> IO ()) -> Sink
sink = Sink

-- | Print an Item to stdout.
printItem :: Sink
printItem = sink print

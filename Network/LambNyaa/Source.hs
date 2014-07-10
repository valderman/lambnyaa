module Network.LambNyaa.Source where
import Network.LambNyaa.Types

-- | Create a Source from a list of items.
list :: [Item] -> Source
list = listIO . return

-- | Create a Source from any IO action returning a list of Items.
listIO :: IO [Item] -> Source
listIO = Source

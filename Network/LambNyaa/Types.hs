module Network.LambNyaa.Types (
    module Item, module Config,
    Source (..)
  ) where
import Control.Monad (mapM_)
import Network.LambNyaa.Item as Item
import Network.LambNyaa.Config as Config

-- | A Source produces Items for consumption by sinks and filters. The simplest
--   Source just produces a static list of Items. More complex ones may
--   produce Items from web search results or RSS feeds.
newtype Source = Source {unSource :: IO [Item]}

module Network.LambNyaa.Types (
    module Item, module Config,
    Source (..),
    modifyRef
  ) where
import Control.Monad (mapM_)
import Network.LambNyaa.Item as Item
import Network.LambNyaa.Config as Config
import Data.IORef

-- | Replacement for atomicModifyIORef' since it doesn't exist with GHC < 7.6.
modifyRef :: IORef a -> (a -> (a, b)) -> IO b
modifyRef r f = do
  x <- atomicModifyIORef r $ \x -> let (a, b) = f x in (a, a `seq` b)
  x `seq` return x

-- | A Source produces Items for consumption by sinks and filters. The simplest
--   Source just produces a static list of Items. More complex ones may
--   produce Items from web search results or RSS feeds.
newtype Source = Source {unSource :: IO [Item]}

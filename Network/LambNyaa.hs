module Network.LambNyaa (
    module Data.Default,
    module Data.Monoid,
    module Control.Applicative,
    module Control.Monad,
    module Network.LambNyaa.Config,
    module Network.LambNyaa.Source.Nyaa,
    module Network.LambNyaa.Source.Feed,
    module Network.LambNyaa.Sink.Download,
    module Network.LambNyaa.Sink.RSS,
    module Network.LambNyaa.Filters,
    module Network.LambNyaa.Scheduler,
    module Network.LambNyaa.Log.Core,
    URL, Item (..), Sink,
    sink, sink_, printItem, seen, unseen,
    Nyaa, source, accept
  ) where
import Data.Default
import Data.Monoid
import Control.Applicative
import Control.Monad
import Network.LambNyaa.Monad
import Network.LambNyaa.Config
import Network.LambNyaa.Item
import Network.LambNyaa.Source.Nyaa
import Network.LambNyaa.Source.Feed
import Network.LambNyaa.Sink
import Network.LambNyaa.Sink.Download
import Network.LambNyaa.Sink.RSS
import Network.LambNyaa.Scheduler
import Network.LambNyaa.Filters
import Network.LambNyaa.Log.Core hiding (newLogItem, unsafeNewLogItem)
import Network.LambNyaa.Log.IO ()

-- TODO:
--   * Sinks: email
--   * More robust Item extraction from Source.Nyaa
--   * Make the itmIdentifier field untouchable?
--   * Make it impossible to create ItemLike values without an ID.
--   * Don't fill in itmSeenBefore; look it up in monad!
--   * Add tools for working specifically with episodes.

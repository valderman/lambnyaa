module Network.LambNyaa (
    module Data.Default,
    module Data.Monoid,
    module Network.LambNyaa.Config,
    module Network.LambNyaa.Source,
    module Network.LambNyaa.Source.Nyaa,
    module Network.LambNyaa.Sink.Download,
    module Network.LambNyaa.Filters,
    module Network.LambNyaa.Scheduler,
    URL, Item (..), Action, Sink, Source,
    sink, sink_, printItem, seen, unseen
  ) where
import Data.Default
import Data.Monoid
import Network.LambNyaa.Config hiding (Action, Sink, Source)
import Network.LambNyaa.Types
import Network.LambNyaa.Source
import Network.LambNyaa.Source.Nyaa
import Network.LambNyaa.Sink
import Network.LambNyaa.Sink.Download
import Network.LambNyaa.Scheduler
import Network.LambNyaa.Filters

-- TODO:
--   * Sinks: email, RSS
--   * Sources: RSS
--   * More robust Item extraction from Source.Nyaa
--   * Remember and disregard previously seen Items (SQLite?)
--   * Properly Haddock everything

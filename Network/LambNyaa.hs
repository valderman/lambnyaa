module Network.LambNyaa (
    module Data.Default,
    module Data.Monoid,
    module Network.LambNyaa.Config,
    module Network.LambNyaa.Source,
    module Network.LambNyaa.Source.Nyaa,
    module Network.LambNyaa.Source.RSS,
    module Network.LambNyaa.Sink.Download,
    module Network.LambNyaa.Sink.RSS,
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
import Network.LambNyaa.Source.RSS
import Network.LambNyaa.Sink
import Network.LambNyaa.Sink.Download
import Network.LambNyaa.Sink.RSS
import Network.LambNyaa.Scheduler
import Network.LambNyaa.Filters

-- TODO:
--   * Sinks: email
--   * More robust Item extraction from Source.Nyaa

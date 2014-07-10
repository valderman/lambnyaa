module Network.LambNyaa.Config (
    Config, TimeUnit (..), Schedule (..),
    Action (..), Sink (..), Filter,
    cfgSources, cfgFilters, cfgSchedule, cfgDatabase,
    def
  ) where
import Data.Default
import {-# SOURCE #-} Network.LambNyaa.Types
import Network.LambNyaa.Item
import Network.LambNyaa.FiniteChan
import System.IO.Unsafe
import System.Directory
import System.FilePath
import Data.IORef
import Control.Monad

-- | A unit of time.
data TimeUnit = Seconds | Minutes | Hours | Days

-- | A run may be scheduled either once, or every n units of time.
--   For instance, Every 10 Minutes.
data Schedule = Once | Every Int TimeUnit

-- | LambNyaa configuration.
data Config = Config {
    cfgSources   :: [Source], -- ^ Sources to fetch items from.
    cfgFilters   :: [Filter], -- ^ Filters to be applied to each Item.
    cfgSchedule  :: Schedule, -- ^ How often should runs recur, if at all?
    cfgDatabase  :: FilePath  -- ^ Which SQLite database file should be used
                              --   for persistent data?
  }

instance Default Config where
  def = Config {
      cfgSources   = [],
      cfgFilters   = [],
      cfgSchedule  = Once,
      cfgDatabase  = defaultDB
    }

{-# NOINLINE defaultDB #-}
-- | Default database file; creates prerequisite directories if they don't
--   exist.
defaultDB :: FilePath
defaultDB = unsafePerformIO $ do
  dir <- getAppUserDataDirectory "lambnyaa"
  d <- doesDirectoryExist dir
  when (not d) $ createDirectory dir
  return $ dir </> "database.sqlite"

-- | Actions decide what happens to an Item after passing through a filter.
--   An Item may have one of two possible fates: either it is accepted into a
--   Sink, or it is passed to the next filter in the pipeline. Discarding an
--   item is implemented by accepting it into a no-op Sink.
data Action = Accept Sink (IO ()) | Pass Item

-- | Filters are used to decide which Items are accepted into which sinks.
--   A Filter may accept or discard items, removing them from the stream,
--   or alter any aspect of an Item.
type Filter = Item -> Action

-- | A Sink is the endpoint of a stream. It consists of an IO action taking a
--   list of Items as its input, contains all Items accepted into the sink.
data Sink = Sink {
    sinkHandler :: Config -> IO (),
    sinkChan    :: IORef (Chan Item),
    writeSink   :: Item -> IO ()
  }

instance Eq Sink where
  a == b = sinkChan a == sinkChan b

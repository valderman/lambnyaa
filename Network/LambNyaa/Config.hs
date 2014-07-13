module Network.LambNyaa.Config (
    Config, TimeUnit (..), Schedule (..),
    Action (..), Sink (..), Filter,
    LogHandler, LogLevel,
    cfgSources, cfgFilters, cfgSchedule, cfgDatabase, cfgLogHandlers,
    cfgLogLevel, cfgCatchSignals,
    def
  ) where
import Data.Default
import {-# SOURCE #-} Network.LambNyaa.Types
import Network.LambNyaa.Item
import Network.LambNyaa.Log
import System.IO.Unsafe
import System.Directory
import System.FilePath
import Control.Monad

-- | A unit of time.
data TimeUnit = Seconds | Minutes | Hours | Days

-- | A run may be scheduled either once, or every n units of time.
--   For instance, Every 10 Minutes.
data Schedule = Once | Every Int TimeUnit

-- | LambNyaa configuration.
data Config = Config {
    cfgSources      :: [Source],    -- ^ Sources to fetch items from.
                                    --   Default: []
    cfgFilters      :: [Filter],    -- ^ Filters to be applied to each Item.
                                    --   Default: []
    cfgSchedule     :: Schedule,    -- ^ How often should runs recur, if at all?
                                    --   Default: Once
    cfgDatabase     :: FilePath,    -- ^ Which SQLite database file should be
                                    --   used for persistent data?
                                    --   Default: ~/.lambnyaa/database.sqlite
    cfgLogHandlers  :: [LogHandler],-- ^ How should data be logged?
                                    --   Default: logToStderr
    cfgLogLevel     :: LogLevel,    -- ^ How much data should be logged?
                                    --   Default: Info
    cfgCatchSignals :: Bool         -- ^ Do a clean exit on SIGINT or SIGTERM?
                                    --   Default: True
  }

instance Default Config where
  def = Config {
      cfgSources      = [],
      cfgFilters      = [],
      cfgSchedule     = Once,
      cfgDatabase     = defaultDB,
      cfgLogHandlers  = [logToStderr],
      cfgLogLevel     = Info,
      cfgCatchSignals = True
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
data Action = Accept [Sink] Item | Pass Item

-- | Filters are used to decide which Items are accepted into which sinks.
--   A Filter may accept or discard items, removing them from the stream,
--   or alter any aspect of an Item.
type Filter = Item -> Action

-- | A Sink is the endpoint of a stream. It consists of an IO action taking a
--   list of Items as its input, contains all Items accepted into the sink.
data Sink = Sink {
    sinkHandler :: Config -> [Item] -> IO (),
    sinkID      :: Int
  }

instance Eq Sink where
  a == b = sinkID a == sinkID b

instance Ord Sink where
  a `compare` b = sinkID a `compare` sinkID b
  a > b         = sinkID a > sinkID b
  a >= b        = sinkID a >= sinkID b
  a < b         = sinkID a < sinkID b
  a <= b        = sinkID a <= sinkID b

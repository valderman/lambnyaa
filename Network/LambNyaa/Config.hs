module Network.LambNyaa.Config (
    Config, TimeUnit (..), Schedule (..),
    cfgSources, cfgFilters, cfgSchedule, cfgDatabase
  ) where
import Data.Default
import Network.LambNyaa.Types
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

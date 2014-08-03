module Network.LambNyaa.Config (
    Config, TimeUnit (..), Schedule (..),
    LogHandler, LogLevel,
    cfgSchedule, cfgDatabase, cfgLogHandlers, cfgLogLevel, cfgCatchSignals,
    def
  ) where
import Data.Default
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

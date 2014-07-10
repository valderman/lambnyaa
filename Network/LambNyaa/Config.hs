module Network.LambNyaa.Config (
    Config, TimeUnit (..), Schedule (..),
    cfgSources, cfgFilters, cfgSchedule
  ) where
import Data.Default
import Network.LambNyaa.Types

data TimeUnit = Seconds | Minutes | Hours | Days
data Schedule = Once | Every Int TimeUnit

data Config = Config {
    cfgSources   :: [Source],
    cfgFilters   :: [Filter],
    cfgSchedule  :: Schedule
  }

instance Default Config where
  def = Config {
      cfgSources   = [],
      cfgFilters   = [],
      cfgSchedule  = Once
    }

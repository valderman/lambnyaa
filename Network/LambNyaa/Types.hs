module Network.LambNyaa.Types where
import Control.Monad (mapM_)
import Data.Monoid

type URL = String

data Item = Item {
    itmName        :: String,
    itmURL         :: URL,
    itmSource      :: String,
    itmDescription :: Maybe String,
    itmTags        :: [String]
  } deriving Show

newtype Sink = Sink {unSink ::  (Item -> IO ())}
newtype Source = Source {unSource :: IO [Item]}
data Action = Accept (IO ()) | Pass Item
type Filter = Item -> Action

instance Monoid Sink where
  mempty = Sink $ \_ -> return ()
  mappend (Sink a) (Sink b) = Sink $ \i -> a i >> b i
  mconcat fs = Sink $ \i -> mapM_ (\(Sink f) -> f i) fs

module Network.LambNyaa.Types where
import Network.LambNyaa.Item

newtype Source = Source {unSource :: IO [Item]}

module Network.LambNyaa.Sink.Download where
import Network.LambNyaa.Types
import Network.LambNyaa.Sink
import Network.Download
import System.FilePath
import qualified Data.ByteString as BS

download :: FilePath -> Sink
download dir = sink $ \item -> do
  ef <- openURI (itmURL item)
  case ef of
    Right f -> BS.writeFile (dir </> takeFileName (itmURL item)) f
    _       -> return ()

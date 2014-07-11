-- | Downloader sink; download files associated with Items.
module Network.LambNyaa.Sink.Download where
import Network.LambNyaa.Types
import Network.LambNyaa.Sink
import Network.Download
import System.FilePath
import qualified Data.ByteString as BS

-- | Download a file into <given directory>/<item name>.<given extension>
download :: FilePath -> String -> Sink
download dir ext = download' $ \item ->
  dir </> sanitize (itmName item) <.> ext

-- | Sink that downloads the URL associated with a given Item.
download' :: (Item -> FilePath) -> Sink
download' mkPath = sink_ . mapM_ $ \item -> do
  ef <- openURI (itmURL item)
  case ef of
    Right f -> BS.writeFile (mkPath item) f
    _       -> return ()

-- | Turn path separator characters into spaces.
sanitize :: FilePath -> FilePath
sanitize = map go
  where
    go c | isPathSeparator c = ' '
         | otherwise         = c

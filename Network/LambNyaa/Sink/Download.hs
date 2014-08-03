-- | Downloader sink; download files associated with Items.
module Network.LambNyaa.Sink.Download where
import Network.LambNyaa.Item
import Network.LambNyaa.Sink
import Network.LambNyaa.Log
import Network.Download
import System.FilePath
import qualified Data.ByteString as BS

err' = err "Sink.Download"
info' = info "Sink.Download"

-- | Download a file into <given directory>/<item name>.<given extension>
download :: FilePath -> String -> Sink
download dir ext = download' $ \item ->
  dir </> sanitize (itmName item) <.> ext

-- | Sink that downloads the URL associated with a given Item.
download' :: (Item -> FilePath) -> Sink
download' mkPath = sink_ . mapM_ $ \item -> do
  ef <- openURI (itmURL item)
  case ef of
    Right f -> do
      info' $ "Saving '" ++ itmURL item ++ "' to '" ++ mkPath item ++ "'."
      BS.writeFile (mkPath item) f
    _       -> do
      err' $ "Unable to fetch '" ++ itmURL item ++ "'!"

-- | Turn path separator characters into spaces.
sanitize :: FilePath -> FilePath
sanitize = map go
  where
    go c | isPathSeparator c = ' '
         | otherwise         = c

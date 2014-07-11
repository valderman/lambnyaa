-- | RSS 2.0 feed source.
--   Items are produced from RSS 2.0 feeds.
--   The itmName, itmURL and itmDescription fields are filled from the
--   corresponding RSS fields.
--   The itmTags field is left empty.
--
--   TODO: support for other feed formats.
module Network.LambNyaa.Source.Feed (rssFeed, rssFeed') where
import Network.LambNyaa.Types
import Network.LambNyaa.Source
import Text.RSS.Syntax
import Text.Feed.Types hiding (Item)
import Network.Download

-- | Create a Source from an RSS feed. The itmSource field of Items originating
--   from this Source will contain the URL of the feed.
rssFeed :: URL -> Source
rssFeed url = rssFeed' url url

-- | Create a Source from a named RSS feed. The itmSource field of Items
--   originating from this Source will contain the given name.
rssFeed' :: String -> URL -> Source
rssFeed' src url = listIO $ do
  ef <- openAsFeed url
  case ef of
    Right (RSSFeed rss) -> return $ getItems src (rssChannel rss)
    _                   -> return []

getItems :: String -> RSSChannel -> [Item]
getItems src = map (getItem src) . rssItems

getItem :: String -> RSSItem -> Item
getItem src item = Item {
    itmName        = maybe "" id (rssItemTitle item),
    itmURL         = maybe "" id (rssItemLink item),
    itmDescription = rssItemDescription item,
    itmSource      = src,
    itmTags        = []
  }

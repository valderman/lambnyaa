-- | RSS 2.0 feed source.
--   Items are produced from RSS 2.0 feeds.
--   The itmName, itmURL and itmDescription fields are filled from the
--   corresponding RSS fields.
--   The itmTags field is populated from the RSS tags.
--
--   TODO: support for other feed formats.
module Network.LambNyaa.Source.Feed (rssFeed, rssFeed') where
import Network.LambNyaa.Item
import Network.LambNyaa.Monad
import Network.LambNyaa.Log
import Text.RSS.Syntax
import Text.Feed.Types hiding (Item)
import Network.Download
import Control.Monad

warn' = warn "Source.Feed"
err' = err "Source.Feed"

-- | Create a Source from an RSS feed. The itmSource field of Items originating
--   from this Source will contain the URL of the feed.
rssFeed :: URL -> Nyaa [Item]
rssFeed url = rssFeed' url url

-- | Create a Source from a named RSS feed. The itmSource field of Items
--   originating from this Source will contain the given name.
rssFeed' :: String -> URL -> Nyaa [Item]
rssFeed' src url = source $ do
  ef <- openAsFeed url
  case ef of
    Right (RSSFeed rss) -> do
      let is = getItems src (rssChannel rss)
      when (null is) . warn' $ "No RSS items from feed " ++ url ++ "!"
      return is
    _ -> do
      err' $ "Unable to parse RSS feed from " ++ url ++ "!"
      return []

getItems :: String -> RSSChannel -> [Item]
getItems src = map (getItem src) . rssItems

getItem :: String -> RSSItem -> Item
getItem src item = Item {
    itmIdentifier  = undefined,
    itmSeenBefore  = undefined,
    itmName        = maybe "" id (rssItemTitle item),
    itmURL         = maybe "" id (rssItemLink item),
    itmDescription = rssItemDescription item,
    itmSource      = src,
    itmTags        = map rssCategoryValue $ rssItemCategories item
  }

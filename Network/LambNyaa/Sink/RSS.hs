-- | Sink that creates a RSS feed from received Items.
module Network.LambNyaa.Sink.RSS (withRSS, withRSS') where
import Network.LambNyaa.Types
import Network.LambNyaa.Sink
import Text.RSS.Syntax

-- | Sink which converts all Items into an RSS feed and then passes that feed
--   to a user-supplied action.
withRSS :: (RSS -> IO ()) -> Sink
withRSS = withRSS' "" "" ""

-- | Like @withRSS@, but with a title, description and URL for the feed.
withRSS' :: String -> String -> URL -> (RSS -> IO ()) -> Sink
withRSS' title desc url act = sink $ \cfg is -> do
  act $ RSS {
      rssVersion = "2.0",
      rssAttrs   = [],
      rssOther   = [],
      rssChannel = RSSChannel {
          rssTitle = title,
          rssDescription = desc,
          rssLink = url,
          rssItems = map toRSSItem is,
          rssLanguage = Nothing,
          rssCopyright = Nothing,
          rssEditor = Nothing,
          rssWebMaster = Nothing,
          rssPubDate = Nothing,
          rssLastUpdate = Nothing,
          rssCategories = [],
          rssGenerator = Just "LambNyaa",
          rssDocs = Nothing,
          rssCloud = Nothing,
          rssTTL = scheduleToTTL cfg,
          rssImage = Nothing,
          rssRating = Nothing,
          rssTextInput = Nothing,
          rssSkipHours = Nothing,
          rssSkipDays = Nothing,
          rssChannelOther = []
        }
    }

-- | Make a TTL for a feed, expressed in minutes, based on how often
--   rescheduling happens. If rescheduling only happens every n days,
--   TTL is set to 12 hours, to avoid excessive caching in cases where an
--   update happens just after the feed is fetched.
scheduleToTTL :: Config -> Maybe Integer
scheduleToTTL cfg =
  case cfgSchedule cfg of
    Once            -> Nothing
    Every n Seconds -> Just 1
    Every n Minutes -> Just $ fromIntegral n
    Every n Hours   -> Just $ fromIntegral (60* n)
    _               -> Just $ fromIntegral (60*12)

toRSSItem :: Item -> RSSItem
toRSSItem item = RSSItem {
    rssItemTitle       = Just $ itmName item,
    rssItemLink        = Just $ itmURL item,
    rssItemDescription = itmDescription item,
    rssItemAuthor      = Nothing,
    rssItemCategories  = map toRSSCat $ itmTags item,
    rssItemComments    = Nothing,
    rssItemEnclosure   = Nothing,
    rssItemGuid        = Nothing,
    rssItemPubDate     = Nothing,
    rssItemSource      = Nothing,
    rssItemAttrs       = [],
    rssItemOther       = []
  }

toRSSCat :: String -> RSSCategory
toRSSCat tag = RSSCategory {
    rssCategoryDomain = Nothing,
    rssCategoryAttrs  = [],
    rssCategoryValue  = tag
  }

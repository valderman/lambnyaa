-- | Sink that creates a RSS feed from received Items.
module Network.LambNyaa.Sink.RSS (
    withRSS, withRSS',
    writeRSS, writeRSS',
    updateRSS, updateRSS'
  ) where
import Network.LambNyaa.Types
import Network.LambNyaa.Sink
import System.Directory
import System.IO
import Text.RSS.Syntax
import Text.RSS.Import
import Text.RSS.Export
import Text.XML.Light

-- | Default title for RSS feeds.
defTitle :: String
defTitle = "LambNyaa RSS feed"

-- | Write the RSS feed to a file, overwriting the file if it already exists.
writeRSS :: FilePath -> Sink
writeRSS = writeRSS' defTitle "" ""

-- | @writeRSS@ but with title, description and URL.
writeRSS' :: String -> String -> URL -> FilePath -> Sink
writeRSS' title desc url f =
  withRSS' title desc url $ writeFile f . ppTopElement . xmlRSS

-- | Prepend the RSS feed to an existing file. Acts as @writeRSS@ if the file
--   does not exist or is not a valid RSS feed.
updateRSS :: FilePath -> Sink
updateRSS = updateRSS' defTitle "" ""

-- | @updateRSS@ but with title, description and URL.
updateRSS' :: String -> String -> URL -> FilePath -> Sink
updateRSS' title desc url f =
    withRSS' title desc url $ \rss -> do
      ex <- doesFileExist f
      if ex then updateFile rss
            else writeFile f $ ppTopElement $ xmlRSS rss
  where
    updateFile rss = do
      txt <- withFile f ReadMode $ \h -> do
        txt <- hGetContents h
        length txt `seq` return txt
      case parseXMLDoc txt of
        Just e | Just oldrss <- elementToRSS e -> do
          writeFile f $ ppTopElement $ xmlRSS $ oldrss `append` rss
        _ -> do
          writeFile f $ ppTopElement $ xmlRSS rss

    append a b =
      a {
          rssChannel = (rssChannel a) {
               rssItems = rssItems (rssChannel a) ++ rssItems (rssChannel b)
             }
        }

-- | Sink which converts all Items into an RSS feed and then passes that feed
--   to a user-supplied action.
withRSS :: (RSS -> IO ()) -> Sink
withRSS = withRSS' defTitle "" ""

-- | Like @withRSS@, but with a title, description and URL for the feed.
withRSS' :: String -> String -> URL -> (RSS -> IO ()) -> Sink
withRSS' title desc url act =
  sink $ \cfg is -> act $ toRSS cfg is title desc url

toRSS :: Config -> [Item] -> String -> String -> URL -> RSS
toRSS cfg is title desc url =
  RSS {
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

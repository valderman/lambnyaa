-- | NyaaTorrents downloader source.
--   Items are produced from scraping the Nyaa search page.
--   Torrent titles end up in the itmName field of the produced Items, and any
--   phrases within square brackets go into the itmTags field.
module Network.LambNyaa.Source.Nyaa (
    NyaaCat (..), Language (..), Software (..),
    nyaaSearch, anime, live, literature, apps, games
  ) where
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Char (isSpace)
import Control.Applicative
import Control.Monad
import Network.Download
import Text.HTML.TagSoup
import Network.LambNyaa.Item
import Network.LambNyaa.Monad
import Network.LambNyaa.Parser
import Network.LambNyaa.Log

-- | Search for anime in a specific language.
anime :: Language -> Maybe NyaaCat
anime = Just . Anime . Just

-- | Search for literature in a specific language.
literature :: Language -> Maybe NyaaCat
literature = Just . Literature . Just

-- | Search for live action in a specific language.
live :: Language -> Maybe NyaaCat
live = Just . LiveAction . Just

-- | Search for games.
games :: Maybe NyaaCat
games = Just $ Software $ Just Games

-- | Search for applications.
apps :: Maybe NyaaCat
apps = Just $ Software $ Just Games

-- | Narrow down searches based on medium and language.
data NyaaCat
  = Anime      (Maybe Language)
  | Audio
  | Literature (Maybe Language)
  | LiveAction (Maybe Language)
  | Pictures
  | Software   (Maybe Software)
  deriving (Show, Eq)

data Language = English | Japanese | Other
  deriving (Show, Eq)
data Software = Apps | Games
  deriving (Show, Eq)

toQuery :: Maybe NyaaCat -> String
toQuery Nothing = "0_0"
toQuery (Just what) =
  case what of
    Anime Nothing              -> "1_0"
    Anime (Just English)       -> "1_37"
    Anime (Just Japanese)      -> "1_11"
    Anime (Just Other)         -> "1_38"
    Audio                      -> "3_0"
    Literature Nothing         -> "2_0"
    Literature (Just English)  -> "2_12"
    Literature (Just Japanese) -> "2_13"
    Literature (Just Other)    -> "2_39"
    LiveAction Nothing         -> "5_0"
    LiveAction (Just English)  -> "5_19"
    LiveAction (Just Japanese) -> "5_20"
    LiveAction (Just Other)    -> "5_21"
    Pictures                   -> "4_0"
    Software Nothing           -> "6_0"
    Software (Just Apps)       -> "6_23"
    Software (Just Games)      -> "6_24"

err'  = err "Source.Nyaa"
warn' = warn "Source.Nyaa"

-- | Create an Item source from a Nyaa search. This relies on parsing the HTML
--   of the Nyaa search page, so it'll break any time their search page gets an
--   overhaul.
nyaaSearch :: Maybe NyaaCat -> String -> Nyaa [Item]
nyaaSearch cat s = source $ do
  ets <- openAsTags $ concat ["http://www.nyaa.se/?page=search&cats=",
                              toQuery cat, "&term=", encode s]
  case ets of
    Right es -> do
      let is = getItems es
      when (null is) . warn' $ "No search results for '" ++ s ++ "'!"
      return is
    _        -> do
      err' $ "Couldn't parse search results for '" ++ s ++ "'!"
      return []

-- | Encode a string into a format suitable for searching Nyaa.
--   TODO: will currently barf on /, # and other web-special chars.
encode :: String -> String
encode s = [if c == ' ' then '+' else c | c <- s]

-- | Extract all Items from a list of tags.
getItems :: [Tag String] -> [Item]
getItems = catMaybes . map extract . splitByViewLinks

-- | Attempt to extract an Item from a list of tags.
--   TODO: extraction could be slightly more robust.
extract :: [Tag String] -> Maybe Item
extract (_:name:_:_:_:dl:_) | isTagOpenName "a" dl =
  Just $ mkItem (innerText [name]) (fromAttrib "href" dl)
extract _ =
  Nothing

-- | Split tag list on each "view torrent" link
splitByViewLinks :: [Tag String] -> [[Tag String]]
splitByViewLinks =
  partitions $ \t -> isTagOpenName "a" t && isViewURL (fromAttrib "href" t)

mkItem :: String -> URL -> Item
mkItem title url = Item {
      itmIdentifier  = undefined,
      itmSeenBefore  = undefined,
      itmName        = name,
      itmURL         = url,
      itmSource      = "Nyaa",
      itmDescription = Nothing,
      itmTags        = tags
    }
  where
    (name, tags) = maybe (title, []) id $ parseName title

-- | Parse a title into a show name and a set of tags.
parseName :: String -> Maybe (String, [String])
parseName = runParser item . map (\c -> if c == '_' then ' ' else c)
  where
    tag = char '[' *> atLeast 1 (charP (/= ']')) <* char ']' <* whitespace
    item = do
      ts1 <- atLeast 0 tag
      name <- atLeast 1 $ charP (/= '[')
      ts2 <- atLeast 0 tag
      atLeast 0 $ charP (const True)
      return (strip name, ts1 ++ ts2)

-- | Strip leading and trailing spaces.
strip :: String -> String
strip = stripRight . stripLeft
  where
    stripLeft (' ':s) = stripLeft s
    stripLeft s       = s
    stripRight [' '] = []
    stripRight (c:s) = c : stripRight s
    stripRight _     = []

isViewURL :: String -> Bool
isViewURL url = "http://www.nyaa.se/?page=view" `isPrefixOf` url

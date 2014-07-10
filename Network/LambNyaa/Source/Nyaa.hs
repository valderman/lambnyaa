-- | NyaaTorrents downloader source.
--   Items are produced from scraping the Nyaa search page.
--   Torrent titles end up in the itmName field of the produced Items, and any
--   phrases within square brackets go into the itmTags field.
module Network.LambNyaa.Source.Nyaa (nyaaSearch) where
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Char (isSpace)
import Control.Applicative
import Network.Download
import Text.HTML.TagSoup
import Network.LambNyaa.Types
import Network.LambNyaa.Parser
import Network.LambNyaa.Source

-- | Create an Item source from a Nyaa search. This relies on parsing the HTML
--   of the Nyaa search page, so it'll break any time their search page gets an
--   overhaul.
nyaaSearch :: String -> Source
nyaaSearch s = listIO $ do
  ets <- openAsTags $ "http://www.nyaa.se/?page=search&term=" ++ encode s
  case ets of
    Right es -> return $ getItems es
    _        -> return []

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
      itmName        = name,
      itmURL         = url,
      itmSource      = "Nyaa",
      itmDescription = Nothing,
      itmTags        = tags,
      itmSeenBefore  = False
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

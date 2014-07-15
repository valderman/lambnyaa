{-# LANGUAGE FlexibleInstances #-}
module Network.LambNyaa.Filters where
import Data.Monoid
import Network.LambNyaa.Types
import Network.LambNyaa.Sink
import Data.Char
import Data.List

-- | Connect two filters in sequence; items only pass to the second filter if
--   they are not accepted or discarded by the first.
(>>>) :: Filter -> Filter -> Filter
a >>> b = \i ->
  case a i of
    Pass i' -> b i'
    a'      -> a'

class Sinks s where
  -- | Accept an Item into the given sink or sinks.
  accept :: s -> Filter

instance Sinks Sink where
  accept s = Accept [s]

instance Sinks [Sink] where
  accept = Accept

-- | Pass over an Item, allowing the next stage in the pipeline to decide
--   whether it should be accepted or not.
pass :: Filter
pass = Pass

-- | Discard an Item from the stream. It will not be accepted into any Sink.
discard :: Filter
discard = accept . sink_ . const $ return ()

-- | Apply a function to each Item in the stream.
mapItem :: (Item -> Item) -> Filter
mapItem f = pass . f

-- | Pass all Items fulfilling a certain predicate to the given Sink.
when :: (Item -> Bool) -> Filter -> Filter
when p f = \i -> if p i then f i else pass i

-- | Pass all Items NOT fulfilling a certain predicate to the given Sink.
unless :: (Item -> Bool) -> Filter -> Filter
unless p s = when (not . p) s

-- | Has this Item been seen before?
seenBefore :: Item -> Bool
seenBefore = itmSeenBefore

-- | True for all Items which have the given String in their itmTags list.
--   Case sensitive.
tagged :: String -> Item -> Bool
tagged tag i = tag `elem` itmTags i

-- | True for all Items with a name equal to the given String. Case insensitive.
named :: String -> Item -> Bool
named name i = name == map toLower (itmName i)

-- | True for all Items where the given string is a prefix to its name.
--   Case insensitive.
hasPrefix :: String -> Item -> Bool
hasPrefix p i = map toLower p `isPrefixOf` map toLower (itmName i)

-- | True for all Items where the title ends in " - <number>". Underscores are
--   interpreted as spaces.
--   TODO: add SxEy and 01v2 conventions to this predicate.
isEpisode :: Item -> Bool
isEpisode i =
  case reverse $ words $ map (\c -> if c == '_' then ' ' else c) $ itmName i of
    (s : "-" : _) -> all isDigit s
    _             -> False

-- | Disjunction for predicates.
(<|>) :: (Item -> Bool) -> (Item -> Bool) -> Item -> Bool
a <|> b = \i -> a i || b i

-- | Conjunction for predicates.
(<&>) :: (Item -> Bool) -> (Item -> Bool) -> Item -> Bool
a <&> b = \i -> a i && b i

-- | Negation for predicates.
not_ :: (Item -> Bool) -> Item -> Bool
not_ = fmap not

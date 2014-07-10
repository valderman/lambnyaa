module Network.LambNyaa.Filters where
import Data.Monoid
import Network.LambNyaa.Types
import Network.LambNyaa.Sink

-- | Connect two filters.
(>>>) :: Filter -> Filter -> Filter
a >>> b = \i ->
  case a i of
    Pass i' -> b i'
    a'      -> a'

-- | Accept an Item into the given sink.
accept :: Sink -> Filter
accept s i = Accept s i

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
tagged :: String -> Item -> Bool
tagged tag i = tag `elem` itmTags i

-- | True for all Items with a name equal to the given String.
named :: String -> Item -> Bool
named name i = name == itmName i

-- | Disjunction for predicates.
(<|>) :: (Item -> Bool) -> (Item -> Bool) -> Item -> Bool
a <|> b = \i -> a i || b i

-- | Conjunction for predicates.
(<&>) :: (Item -> Bool) -> (Item -> Bool) -> Item -> Bool
a <&> b = \i -> a i && b i

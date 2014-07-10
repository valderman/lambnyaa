module Network.LambNyaa.Filters where
import Network.LambNyaa.Types
import Network.LambNyaa.Sink

-- | Pass all Items to the given Sink.
always :: Sink -> Filter
always s = const s

-- | Pass all Items fulfilling a certain predicate to the given Sink.
when :: (Item -> Bool) -> Sink -> Filter
when p s = \i -> if p i then s else ignore

-- | Pass all Items NOT fulfilling a certain predicate to the given Sink.
unless :: (Item -> Bool) -> Sink -> Filter
unless p s = when (not . p) s

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

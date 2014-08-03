{-# LANGUAGE FlexibleInstances #-}
module Network.LambNyaa.Filters where
import Data.Monoid
import Network.LambNyaa.Item
import Network.LambNyaa.Sink
import Data.Char
import Data.List

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
(.|.) :: (Item -> Bool) -> (Item -> Bool) -> Item -> Bool
a .|. b = \i -> a i || b i

-- | Conjunction for predicates.
(.&.) :: (Item -> Bool) -> (Item -> Bool) -> Item -> Bool
a .&. b = \i -> a i && b i

-- | Negation for predicates.
not_ :: (Item -> Bool) -> Item -> Bool
not_ = fmap not

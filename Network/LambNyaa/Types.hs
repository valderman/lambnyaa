module Network.LambNyaa.Types where
import Control.Monad (mapM_)
import Data.Monoid
import Data.Hashable

-- | Vanity type for URLs.
type URL = String

-- | A data item. What, exactly, a data item represents depends on the source.
data Item = Item {
    itmIdentifier  :: Int,      -- ^ A unique identifier for Items, used to
                                --   decide whether an Item has been previously
                                --   encountered. This value is a hash of the
                                --   itmName, itmURL, itmTags and
                                --   itmDescription fields.
    itmName        :: String,   -- ^ The human readable name of the item.
    itmURL         :: URL,      -- ^ The URL associated with an item.
    itmSource      :: String,   -- ^ Name of the source that produced the item.
    itmDescription :: Maybe String, -- ^ An optional plaintext description of
                                    --   the item.
    itmTags        :: [String], -- ^ A list of tags that somehow apply to
                                --   the item. How this is filled is up to the
                                --   Source that produces it. For instance, the
                                --   NyaaTorrents source treats each phrase
                                --   inside square brackets within an item name
                                --   as a tag, to create tags for fansub group
                                --   names, resolution, checksum, etc.
    itmSeenBefore :: Bool       -- ^ Has this item been seen before? This field
                                --   is filled in automatically.
  } deriving Show

instance Hashable Item where
  hashWithSalt salt item =
    salt         `hashWithSalt`
    itmName item `hashWithSalt`
    itmURL item  `hashWithSalt`
    itmTags item `hashWithSalt` itmDescription item

-- | A Sink is the endpoint of a stream. It consists of an IO action taking an
--   Item as its input, which is executed once for each Item that is accepted
--   into the sink.
newtype Sink = Sink {unSink ::  (Item -> IO ())}

-- | A Source produces Items for consumption by sinks and filters. The simplest
--   Source just produces a static list of Items. More complex ones may
--   produce Items from web search results or RSS feeds.
newtype Source = Source {unSource :: IO [Item]}

-- | Actions decide what happens to an Item after passing through a filter.
--   An Item may have one of two possible fates: either it is accepted into a
--   Sink, or it is passed to the next filter in the pipeline. Discarding an
--   item is implemented by accepting it into a no-op Sink.
data Action = Accept (IO ()) | Pass Item

-- | Filters are used to decide which Items are accepted into which sinks.
--   A Filter may accept or discard items, removing them from the stream,
--   or alter any aspect of an Item.
type Filter = Item -> Action

instance Monoid Sink where
  mempty = Sink $ \_ -> return ()
  mappend (Sink a) (Sink b) = Sink $ \i -> a i >> b i
  mconcat fs = Sink $ \i -> mapM_ (\(Sink f) -> f i) fs

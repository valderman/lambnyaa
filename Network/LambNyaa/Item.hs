module Network.LambNyaa.Item where
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

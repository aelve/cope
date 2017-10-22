-- | Getting entries and other data
module Cope.Query
(
  EntryPointer(..),
  findEntry,
  getEntries,
)
where


import Imports
import qualified Database.Esqueleto as E

import Cope.Types
  

data EntryPointer
  = Index Int   -- ^ Entry with specific index
  deriving (Eq, Show)

-- | Find an entry by 'EntryPointer'.
findEntry :: MonadIO m => EntryPointer -> E.SqlReadT m (EntryId, Entry)
findEntry p@(Index i) = do
  let unwrap (eid, e) = (E.unValue eid, E.entityVal e)
  xs <- fmap (map unwrap) $
    E.select $ E.from $ \entry -> do
      E.orderBy [E.asc (entry E.^. EntryId)]
      E.offset (fromIntegral i)
      E.limit 1
      return (entry E.^. EntryId, entry)
  case xs of
    [x] -> pure x
    []  -> fail ("findEntry: no entries for "+||p||+"")
    _   -> fail ("findEntry: several entries for "+||p||+": "+||xs||+"")

-- | Get a list of all entries.
getEntries :: MonadIO m => E.SqlReadT m [Entry]
getEntries =
  fmap (map E.entityVal) $
    E.select $ E.from $ \entry ->
      return entry

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
  = Index Int   -- ^ Entry with specific index (newest is 1)
  deriving (Eq, Show)

-- | Find an entry by 'EntryPointer'.
findEntry :: MonadIO m => EntryPointer -> E.SqlReadT m (EntryId, Entry)
findEntry p@(Index i) = do
  let unwrap (eid, e) = (E.unValue eid, E.entityVal e)
  xs <- fmap (map unwrap) $
    E.select $ E.from $ \entry -> do
      customOrder entry
      E.offset (fromIntegral i - 1)
      E.limit 1
      return (entry E.^. EntryId, entry)
  case xs of
    [x] -> pure x
    []  -> fail ("findEntry: no entries for "+||p||+"")
    _   -> fail ("findEntry: several entries for "+||p||+": "+||xs||+"")

-- | Get a list of all entries. The entries are sorted from newest to oldest
-- and accompanied with their indices (which could be given to 'Index')
getEntries :: MonadIO m => E.SqlReadT m [(Int, Entry)]
getEntries =
  fmap (zip [1..] . map E.entityVal) $
    E.select $ E.from $ \entry -> do
      customOrder entry
      return entry

-- | Entries are ordered by date of creation (newest first), with “done”
-- entries going to the bottom.
customOrder :: E.SqlExpr (E.Entity Entry) -> E.SqlQuery ()
customOrder entry = do
  E.orderBy [ E.desc (E.isNothing (entry E.^. EntryDone))
            , E.desc (entry E.^. EntryId) ]

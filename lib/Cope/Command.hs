module Cope.Command
(
  EntryPointer(..),
  TimeDescr(..),
  Command(..),
  parseCommand,
  execCommand,
)
where


import Imports
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import qualified Database.Esqueleto as E
import Database.Esqueleto ((=.), (==.))
import qualified Data.Text.All as T

import Cope.Types
import Cope.Query


----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

data TimeDescr
  = Now                      -- ^ “Right now”
  | OnlyTime TimeOfDay       -- ^ Time of day (in the current timezone)
  | TimeAndDate LocalTime    -- ^ Time and date (in the current timezone)
  deriving (Eq, Show)

data Command
  = Add Text                            -- ^ Add a new item
  | Delete EntryPointer                 -- ^ Delete the item
  | SetTitle    EntryPointer Text       -- ^ Set “title”
  | SetWhere    EntryPointer Text       -- ^ Set “where”
  | SetSeen     EntryPointer TimeDescr  -- ^ Set “seen”
  | SetAck      EntryPointer TimeDescr  -- ^ Set “acknowledged”
  | SetDeadline EntryPointer TimeDescr  -- ^ Set “deadline”
  | SetDone     EntryPointer TimeDescr  -- ^ Set “done”
  deriving (Eq, Show)

type Parser = P.Parsec Void Text

----------------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------------

parseCommand :: Text -> Either String Command
parseCommand = over _Left P.parseErrorPretty
             . P.parse (pCommand <* P.eof) ""

pCommand :: Parser Command
pCommand = P.choice $ map P.try
  [ Add <$> (P.string "add " *> P.takeRest)
  , Delete <$> (mbEntryPointer <* P.string "delete")
  , SetTitle
      <$> (mbEntryPointer <* P.string "title ")
      <*> P.takeRest
  , SetWhere
      <$> (mbEntryPointer <* P.string "where ")
      <*> P.takeRest
  , SetSeen
      <$> (mbEntryPointer <* P.string "seen ")
      <*> pTimeDescr
  , SetAck
      <$> (mbEntryPointer <* P.string "ack ")
      <*> pTimeDescr
  , SetDeadline
      <$> (mbEntryPointer <* P.string "deadline ")
      <*> pTimeDescr
  , SetDone
      <$> (mbEntryPointer <* P.string "done ")
      <*> pTimeDescr
  ]
  where
    mbEntryPointer = (pEntryPointer <* P.char '.') <|> pure (Index 0)

pTimeDescr :: Parser TimeDescr
pTimeDescr = P.choice $ map P.try
  [ Now <$ P.string "now"
  , OnlyTime <$> do
      s <- T.toString <$> P.takeRest
      maybe (fail "no time parse") pure $ pTime s
  , TimeAndDate <$> do
      s <- T.toString <$> P.takeRest
      maybe (fail "no time-and-date parse") pure $ pTimeAndDate s
  ]

pEntryPointer :: Parser EntryPointer
pEntryPointer = Index <$> P.decimal

pTime :: String -> Maybe TimeOfDay
pTime s = asum
  [ parseTimeM True defaultTimeLocale "%-H.%-M" s
  , parseTimeM True defaultTimeLocale "%-H:%-M" s
  , parseTimeM True defaultTimeLocale "%-H" s
  , parseTimeM True defaultTimeLocale "%-I.%-M%P" s
  , parseTimeM True defaultTimeLocale "%-I:%-M%P" s
  , parseTimeM True defaultTimeLocale "%-I%P" s
  ]

pTimeAndDate :: String -> Maybe LocalTime
pTimeAndDate s = asum
  [ parseTimeM True defaultTimeLocale "%F %-H.%-M" s
  , parseTimeM True defaultTimeLocale "%F %-H:%-M" s
  , parseTimeM True defaultTimeLocale "%F %-H" s
  , parseTimeM True defaultTimeLocale "%F %-I.%-M%P" s
  , parseTimeM True defaultTimeLocale "%F %-I:%-M%P" s
  , parseTimeM True defaultTimeLocale "%F %-I%P" s
  ]

----------------------------------------------------------------------------
-- Execution
----------------------------------------------------------------------------

execCommand :: MonadIO m => Command -> E.SqlPersistT m ()
execCommand = \case
  -- Add a new entry
  Add title -> do
    E.insert_ $ Entry
      { entryTitle    = title
      , entryWhere    = Nothing
      , entrySeen     = Nothing
      , entryAck      = Nothing
      , entryDeadline = Nothing
      , entryDone     = Nothing
      }

  -- Delete an entry
  Delete pointer -> do
    (entryId, _) <- findEntry pointer
    deleteById entryId

  -- Set the “title” field
  SetTitle pointer title -> do
    updateEntry pointer $ \entry ->
      E.set entry [ EntryTitle =. E.val title ]

  -- Set the “where” field
  SetWhere pointer where_ -> do
    updateEntry pointer $ \entry ->
      E.set entry [ EntryWhere =. E.just (E.val where_) ]

  -- Set the “seen” field
  SetSeen pointer seen -> do
    time <- resolveTimeDescr seen
    updateEntry pointer $ \entry ->
      E.set entry [ EntrySeen =. E.just (E.val time) ]

  -- Set the “acknowledged” field
  SetAck pointer ack -> do
    time <- resolveTimeDescr ack
    updateEntry pointer $ \entry ->
      E.set entry [ EntryAck =. E.just (E.val time) ]

  -- Set the “deadline” field
  SetDeadline pointer deadline -> do
    time <- resolveTimeDescr deadline
    updateEntry pointer $ \entry ->
      E.set entry [ EntryDeadline =. E.just (E.val time) ]

  -- Set the “done” field
  SetDone pointer done -> do
    time <- resolveTimeDescr done
    updateEntry pointer $ \entry ->
      E.set entry [ EntryDone =. E.just (E.val time) ]

  where
    updateEntry pointer upd = do
      (entryId, _) <- findEntry pointer
      updateById entryId upd

resolveTimeDescr :: MonadIO m => TimeDescr -> m UTCTime
resolveTimeDescr = liftIO . \case
  Now -> getCurrentTime
  OnlyTime tod -> do
    now <- zonedTimeToLocalTime <$> getZonedTime
    localToUtc $ now {localTimeOfDay = tod}
  TimeAndDate t -> localToUtc t
  where
    localToUtc t = localTimeToUTC <$> getCurrentTimeZone <*> pure t

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

updateById
  :: (E.PersistEntityBackend val ~ E.SqlBackend,
      MonadIO m, E.PersistEntity val)
  => Key val
  -> (E.SqlExpr (E.Entity val) -> E.SqlQuery a)
  -> E.SqlWriteT m ()
updateById entryId upd = E.update $ \entry -> do
  upd entry
  E.where_ (entry E.^. E.persistIdField ==. E.val entryId)

deleteById
  :: (E.PersistEntityBackend val ~ E.SqlBackend,
      MonadIO m, E.PersistEntity val)
  => Key val
  -> E.SqlWriteT m ()
deleteById entryId = E.delete $ E.from $ \entry ->
  E.where_ (entry E.^. E.persistIdField ==. E.val entryId)

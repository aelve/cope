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

import Cope.Types
import Cope.Query


----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

data TimeDescr
  = Now
  deriving (Eq, Show)

data Command
  = Add Text                            -- ^ Add a new item
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
pTimeDescr = Now <$ P.string "now"

pEntryPointer :: Parser EntryPointer
pEntryPointer = Index <$> P.decimal

----------------------------------------------------------------------------
-- Execution
----------------------------------------------------------------------------

execCommand :: MonadIO m => Command -> E.SqlPersistT m ()
execCommand = \case
  -- Add a new entry
  Add title -> do
    E.insert_ $ Entry
      { entryTitle    = title
      , entrySeen     = Nothing
      , entryAck      = Nothing
      , entryDeadline = Nothing
      , entryDone     = Nothing
      }

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
resolveTimeDescr Now = liftIO getCurrentTime

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

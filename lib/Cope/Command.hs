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
  = Add Text                        -- ^ Add a new item
  | SetSeen EntryPointer TimeDescr  -- ^ Set “seen”
  | SetAck  EntryPointer TimeDescr  -- ^ Set “acknowledged”
  | SetDone EntryPointer TimeDescr  -- ^ Set “done”
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
execCommand (Add title) = do
  E.insert_ $ Entry
    { entryTitle    = title
    , entrySeen     = Nothing
    , entryAck      = Nothing
    , entryDeadline = Nothing
    , entryDone     = Nothing
    }
execCommand (SetSeen pointer seen) = do
  (entryId, _) <- findEntry pointer
  time <- resolveTimeDescr seen
  E.update $ \entry -> do
    E.set entry [ EntrySeen =. E.just (E.val time) ]
    E.where_ (entry E.^. EntryId ==. E.val entryId)
execCommand (SetAck pointer ack) = do
  (entryId, _) <- findEntry pointer
  time <- resolveTimeDescr ack
  E.update $ \entry -> do
    E.set entry [ EntryAck =. E.just (E.val time) ]
    E.where_ (entry E.^. EntryId ==. E.val entryId)
execCommand (SetDone pointer done) = do
  (entryId, _) <- findEntry pointer
  time <- resolveTimeDescr done
  E.update $ \entry -> do
    E.set entry [ EntryDone =. E.just (E.val time) ]
    E.where_ (entry E.^. EntryId ==. E.val entryId)

-- TODO: can I write 'updateById' or something?

resolveTimeDescr :: MonadIO m => TimeDescr -> m UTCTime
resolveTimeDescr Now = liftIO getCurrentTime

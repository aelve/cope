{-# LANGUAGE OverloadedStrings #-}


module Cope.Command
(
  ItemPointer(..),
  TimeDescr(..),
  Command(..),
  parseCommand,
)
where


import Imports
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P


data ItemPointer
  = Index Int   -- ^ Item with specific index
  deriving (Eq, Show)

data TimeDescr
  = Now
  deriving (Eq, Show)

data Command
  = Add Text                       -- ^ Add a new item
  | SetSeen ItemPointer TimeDescr  -- ^ Set “seen”
  | SetAck  ItemPointer TimeDescr  -- ^ Set “acknowledged”
  | SetDone ItemPointer TimeDescr  -- ^ Set “done”
  deriving (Eq, Show)

type Parser = P.Parsec Void Text

parseCommand :: Text -> Either String Command
parseCommand = over _Left P.parseErrorPretty
             . P.parse (pCommand <* P.eof) ""

pCommand :: Parser Command
pCommand = P.choice
  [ Add <$> (P.string "add " *> P.takeRest)
  , SetSeen
      <$> (mbItemPointer <* P.string "seen ")
      <*> pTimeDescr
  , SetAck
      <$> (mbItemPointer <* P.string "ack ")
      <*> pTimeDescr
  , SetDone
      <$> (mbItemPointer <* P.string "done ")
      <*> pTimeDescr
  ]
  where
    mbItemPointer = (pItemPointer <* P.char '.') <|> pure (Index 0)

pTimeDescr :: Parser TimeDescr
pTimeDescr = Now <$ P.string "now"

pItemPointer :: Parser ItemPointer
pItemPointer = Index <$> P.decimal

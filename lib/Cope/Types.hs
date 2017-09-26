{-# LANGUAGE TemplateHaskell #-}


module Cope.Types
(
  Entry(..),
    title,
    seen,
    ack,
    deadline,
    done,
  DB(..),
)
where


import Imports


data Entry = Entry
  { -- | Text of the entry (e.g. “Boss asked a question”)
    _entryTitle :: Text
    -- | When the question/etc was seen (e.g. “I saw his message at
    -- 10.30am”)
  , _entrySeen :: Maybe UTCTime
    -- | When the question was acknowledged (e.g. “at 10.53am I said I will
    -- answer tomorrow”)
  , _entryAck :: Maybe UTCTime
    -- | Deadline for responding (e.g. “tomorrow”)
  , _entryDeadline :: Maybe UTCTime
    -- | When you actually responded (or finished a task, or something)
  , _entryDone :: Maybe UTCTime
  }
  deriving (Eq, Show)

makeFields ''Entry

data DB = DB
  { entries :: [Entry]
  }
  deriving (Eq, Show)

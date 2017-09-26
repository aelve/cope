{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Cope.Types
(
  Entry(..),
  EntryId,
  migrateAll,
)
where


import Imports

import Database.Persist.TH
 

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Entry
    title     Text               -- “Boss wants me to do X”
    seen      UTCTime Maybe      -- “I saw his message at 10.30am”
    ack       UTCTime Maybe      -- “at 10.53am I said I'll do it tomorrow”
    deadline  UTCTime Maybe      --                            -> tomorrow
    done      UTCTime Maybe      -- “I did X at 7pm”
    deriving Show
|]

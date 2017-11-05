{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


-- No explicit export list because Persistent generates lots of stuff,
-- including data families
module Cope.Types where


import Imports

import Database.Persist.TH
import Database.Esqueleto
 

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Entry
    title     Text               -- “Boss wants me to do X”
    where     Text Maybe         -- “he said so in Slack”
    seen      UTCTime Maybe      -- “I saw his message at 10.30am”
    ack       UTCTime Maybe      -- “at 10.53am I said...”
    deadline  UTCTime Maybe      --             “...that I'll do it tomorrow”
    done      UTCTime Maybe      -- “I did X at 7pm the next day”
    deriving Show
|]

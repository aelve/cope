{-# LANGUAGE FlexibleContexts #-}
module Cope.Main
(
  main,
)
where


import Imports

import Database.Persist.Sqlite
import Control.Monad.Logger
import Control.Monad.Trans.Control
import UnliftIO.Exception (catchAny)
import qualified Data.Text.All as T

import Cope.Types
import Cope.Gui
import Cope.Query
import Cope.Command


main :: IO ()
main =
  runStderrLoggingT $
  withSqliteConn ":memory:" $ \conn -> do
    let sql :: MonadBaseControl IO m => ReaderT SqlBackend m a -> m a
        sql action = runSqlConn action conn
    sql $ runMigration migrateAll
    -- initialize and run the GUI
    gui <- createGui
    let refreshEntries = do
          entries <- sql $ getEntries
          time <- liftIO getCurrentTime
          -- Show entries that are either not done or were done recently
          let recent x = diffUTCTime time x <= 3600*24
              ifShow (_, Entry{..}) = maybe True recent entryDone
          setEntries gui (filter ifShow entries)
    bindCommandHandler gui $ \cmdString -> do
      case parseCommand cmdString of
        Left err ->
          showErrorMessage (T.toStrict err)
        Right cmd -> do
          sql (execCommand cmd) `catchAny` \ex ->
            showErrorMessage (T.toStrict (displayException ex))
          clearCommandInput gui
          refreshEntries
    refreshEntries
    runGui gui


{- Small TODOs
~~~~~~~~~~~~~~

* Call 'refreshEntries' after the GUI has been initialized because then we
  can show an error message if it fails
* Better parsing (without 'try' everywhere, with labels, etc)
* Add "Cope.DB" or something that would abstract away getting/setting
* Make the index column smaller

-}

{- Wishes
~~~~~~~~~

* Allow pasting screenshots
* Allow editing data
* Have something other than “now”
* Show dates better
* Show how much time is left to the deadline (as an imprecise diff)
* In “done” show how much time was left to the deadline +
  percentage of the distance between “ack” and “deadline”
* Save to a real database
* Show errors better instead of having a popup
* Have both TODOs and TOANSWERs or smth
* Have analytics
* Allow deleting entries
* Have trees of entries
* Allow aborting things
* Allow showing all entries (or entries in the last month, etc)

-}

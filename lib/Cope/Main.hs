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
main = do
  dbDir <- getAppUserDataDirectory "aelve/cope"
  createDirectoryIfMissing True dbDir
  let dbPath = dbDir </> "data.db"
  runStderrLoggingT $ withSqliteConn (T.toStrict dbPath) $ \conn -> do
    let sql :: MonadBaseControl IO m => ReaderT SqlBackend m a -> m a
        sql action = runSqlConn action conn
    sql $ runMigration migrateAll
    -- initialize and run the GUI
    gui <- createGui
    let refreshEntries = do
          entries <- sql $ getEntries
          let notDone (_, Entry{..}) = isNothing entryDone
          setEntries gui (filter notDone entries)
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
* Allow setting title and “where” in one go
* Show years in small font
* Allow adding with Ctrl+Enter

-}

{- Wishes
~~~~~~~~~

* Allow pasting screenshots
* Allow editing data
* Show dates better
* Show how much time is left to the deadline (as an imprecise diff)
* In “done” show how much time was left to the deadline +
  percentage of the distance between “ack” and “deadline”
* Show errors better instead of having a popup
* Have both TODOs and TOANSWERs or smth
* Have analytics
* Have trees of entries
* Allow aborting things
* Allow showing all entries (or entries in the last month, etc)
* Add a “why afraid to answer” field or smth
* Use IDs internally, not indices
* Add a “tag” field
* Allow searching

-}

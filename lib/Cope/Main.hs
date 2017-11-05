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
          setEntries gui entries
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

-- TODO: we should 'refreshEntries' after the GUI has been initialized
-- because then we can show an error message if it fails

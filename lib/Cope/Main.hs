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
    entries <- sql $ getEntries
    -- initialize and run the GUI
    gui <- createGui
    setEntries gui entries
    bindCommandHandler gui $ \cmdString -> do
      case parseCommand cmdString of
        Left err ->
          showErrorMessage (T.toStrict err)
        Right cmd -> do
          clearCommandInput gui
          sql $ execCommand cmd
    runGui gui

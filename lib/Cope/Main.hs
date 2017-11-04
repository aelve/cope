module Cope.Main
(
  main,
)
where


import Imports

import Database.Persist.Sqlite
import Control.Monad.Logger

import Cope.Types
import Cope.Gui
import Cope.Query


main :: IO ()
main =
  runStderrLoggingT $
  withSqliteConn ":memory:" $ \conn -> do
    runSqlConn (runMigration migrateAll) conn
    gui <- liftIO createGui
    entries <- runSqlConn getEntries conn
    liftIO $ setEntries gui entries
    liftIO $ runGui gui

{-
next steps:
  command execution
  binding command execution to the field
-}


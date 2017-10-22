{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}


module Cope.Main
(
  main,
)
where


import Imports hiding (on)

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.GI.Base


main :: IO ()
main = do
  gui <- createGui
  runGui gui

{-
next steps:
  command execution
  binding command execution to the field
-}


-- | All created GUI objects (to be manipulated by various functions).
data Gui = Gui
  {
  -- | Main window
    mainWindow :: Gtk.Window
  -- | A place to enter commands
  , commandInput :: Gtk.Entry
  -- | List of entries
  , entriesList :: Gtk.TreeView
  }

createGui :: IO Gui
createGui = do
  -- Create a window
  Gtk.init Nothing
  mainWindow <- new Gtk.Window
    [ #title          := "Cope"
    , #gravity        := Gdk.GravityCenter
    , #windowPosition := Gtk.WindowPositionCenter
    , #defaultWidth   := 600
    , #defaultHeight  := 500 ]
  mainWindow `on` #destroy $
    Gtk.mainQuit
  mainWindow `on` #keyPressEvent $ \e -> do
    keyVal  <- Gdk.getEventKeyKeyval e
    keyName <- Gdk.keyvalName keyVal
    if keyName == Just "Escape"
      then #destroy mainWindow >> pure True
      else pure False

  -- Create a text entry
  commandInput <- new Gtk.Entry []

  -- Create a new list model
  entriesModel <- new Gtk.ListStore []
  entriesList <- new Gtk.TreeView
    [ #model          := entriesModel
    , #headersVisible := True ]

  -- Add some columns
  let addColumn title = do
        column <- new Gtk.TreeViewColumn
          [ #reorderable := True
          , #resizable   := True
          , #expand      := True
          , #title       := title ]
        renderer <- new Gtk.CellRendererText []
        #packStart column renderer True
        #appendColumn entriesList column
        pure (column, renderer)

  (col_thing, colRenderer_thing) <- addColumn "Thing"
  (col_seen,  colRenderer_seen)  <- addColumn "Seen"
  (col_ack,   colRenderer_ack)   <- addColumn "Acknowledged"
  (col_done,  colRenderer_done)  <- addColumn "Done"

{-
  cellLayoutSetAttributes columnChar columnRendererChar model $
    \(_mbNote, entity) -> [ cellText := entity ]
  cellLayoutSetAttributes columnDescription columnRendererDescription model $
    \(mbNote, entity) -> [
      cellText := let name = case M.lookup entity names of
                        Nothing -> []
                        Just x  -> [x]
                      note = case mbNote of
                        Nothing -> []
                        Just x  -> ["[" <> x <> "]"]
                  in  T.unwords (name ++ note) ]
  cellLayoutSetAttributes columnUnicodeName columnRendererUnicodeName model $
    \(_mbNote, entity) -> [
      cellText := if T.length entity == 1
                    then T.charName (T.head entity)
                    else "" ]
-}

  viewScrolled <- new Gtk.ScrolledWindow []
  #add viewScrolled entriesList

  -- Create a layout
  layout <- new Gtk.Table
    [ #nRows    := 2
    , #nColumns := 1 ]
  #attach layout commandInput 0 1 0 1
    [Gtk.AttachOptionsFill] [Gtk.AttachOptionsFill] 0 0
  #attachDefaults layout viewScrolled 0 1 1 2
  #add mainWindow layout

  pure Gui
    { mainWindow   = mainWindow
    , commandInput = commandInput
    , entriesList  = entriesList
    }

-- | Run created GUI. This command doesn't return.
runGui :: Gui -> IO ()
runGui Gui{..} = do
  #showAll mainWindow
  Gtk.main

{-# LANGUAGE OverloadedLabels #-}


module Cope.Gui
(
  -- * GUI
  Gui(..),
  createGui,
  runGui,

  -- * Methods
  setEntries,
)
where


import Imports hiding (on)

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.GI.Base
import Data.GI.Base.GValue

import Cope.Types


-- | All created GUI objects (to be manipulated by various functions).
data Gui = Gui
  {
  -- | Main window
    mainWindow :: Gtk.Window
  -- | A place to enter commands
  , commandInput :: Gtk.Entry
  -- | A view for the list of entries
  , entriesList :: Gtk.TreeView
  -- | The actual entries are stored here
  , entriesModel :: Gtk.ListStore
  }

-- | Create all widgets, but don't display them or run the GTK loop.
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
  #setColumnTypes entriesModel
    [gtypeString, gtypeString, gtypeString, gtypeString]
  entriesList <- new Gtk.TreeView
    [ #model          := entriesModel
    , #headersVisible := True ]

  -- Add some columns
  let addColumn i title = do
        column <- new Gtk.TreeViewColumn
          [ #reorderable := True
          , #resizable   := True
          , #expand      := True
          , #title       := title ]
        #appendColumn entriesList column
        renderer <- new Gtk.CellRendererText []
        #packStart column renderer True
        #addAttribute column renderer "text" i
        pure (column, renderer)

  -- NB. if the order or amount of column changes, don't forget to change
  -- 'setEntries' too
  (col_thing, colRenderer_thing) <- addColumn 0 "Thing"
  (col_seen,  colRenderer_seen)  <- addColumn 1 "Seen"
  (col_ack,   colRenderer_ack)   <- addColumn 2 "Acknowledged"
  (col_done,  colRenderer_done)  <- addColumn 3 "Done"

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
    , entriesModel = entriesModel
    }

-- | Run created GUI. This command also starts the GTK loop and therefore
-- doesn't return.
runGui :: Gui -> IO ()
runGui Gui{..} = do
  #showAll mainWindow
  Gtk.main

-- | Set the contents of the entries list.
setEntries :: Gui -> [Entry] -> IO ()
setEntries Gui{..} entries = do
  #clear entriesModel
  for_ entries $ \Entry{..} -> do
    row <- #append entriesModel
    g_title <- toGValue (Just entryTitle)
    g_seen  <- toGValue (show <$> entrySeen)
    g_ack   <- toGValue (show <$> entryAck)
    g_done  <- toGValue (show <$> entryDone)
    #set entriesModel row [0..3] [g_title, g_seen, g_ack, g_done]

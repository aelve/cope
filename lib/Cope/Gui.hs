{-# LANGUAGE OverloadedLabels #-}


module Cope.Gui
(
  -- * GUI
  Gui(..),
  createGui,
  runGui,

  -- * Methods
  setEntries,
  bindCommandHandler,
  clearCommandInput,

  -- * Other stuff
  showErrorMessage,
)
where


import Imports hiding (set, get, on)

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.GI.Base
import UnliftIO

import Cope.Types


----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- GUI initialization
----------------------------------------------------------------------------

-- | Create all widgets, but don't display them or run the GTK loop.
createGui :: MonadIO m => m Gui
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

  -- Create the entries list
  (entriesList, entriesModel) <- createEntriesList
  entriesListScrolled <- new Gtk.ScrolledWindow []
  #add entriesListScrolled entriesList

  -- Create a layout
  layout <- new Gtk.Table
    [ #nRows    := 2
    , #nColumns := 1 ]
  #attach layout commandInput 0 1 0 1
    [Gtk.AttachOptionsFill] [Gtk.AttachOptionsFill] 0 0
  #attachDefaults layout entriesListScrolled 0 1 1 2
  #add mainWindow layout

  pure Gui
    { mainWindow   = mainWindow
    , commandInput = commandInput
    , entriesList  = entriesList
    , entriesModel = entriesModel
    }

-- | Create the entries list.
createEntriesList :: MonadIO m => m (Gtk.TreeView, Gtk.ListStore)
createEntriesList = do
  -- NB. if the order or amount of column changes, don't forget to change
  -- 'setEntries'
  let columns =
        [ (""            , gtypeString)    -- index
        , ("Thing"       , gtypeString)
        , ("Seen"        , gtypeString)
        , ("Acknowledged", gtypeString)
        , ("Deadline"    , gtypeString)
        , ("Done"        , gtypeString)
        ]

  -- Create a new list model
  entriesModel <- new Gtk.ListStore []
  #setColumnTypes entriesModel (map snd columns)
  entriesList <- new Gtk.TreeView
    [ #model          := entriesModel
    , #headersVisible := True ]

  -- Add some columns
  ifor_ columns $ \i (title, _type) -> do
    column <- new Gtk.TreeViewColumn
      [ #reorderable := True
      , #resizable   := True
      , #expand      := True
      , #title       := title ]
    #appendColumn entriesList column
    renderer <- new Gtk.CellRendererText []
    #packStart column renderer True
    #addAttribute column renderer "text" (fromIntegral i)

  pure (entriesList, entriesModel)

----------------------------------------------------------------------------
-- GUI running
----------------------------------------------------------------------------

-- | Run created GUI. This command also starts the GTK loop and therefore
-- doesn't return.
runGui :: MonadIO m => Gui -> m ()
runGui Gui{..} = do
  #showAll mainWindow
  Gtk.main

----------------------------------------------------------------------------
-- GUI methods
----------------------------------------------------------------------------

-- | Set the contents of the entries list.
setEntries :: MonadIO m => Gui -> [Entry] -> m ()
setEntries Gui{..} entries = liftIO $ do
  #clear entriesModel
  ifor_ entries $ \i Entry{..} -> do
    row <- #append entriesModel
    let showTime utcT = do
          localT <- utcToLocalZonedTime utcT
          return (format "{} {}" (dateDashF localT) (hmF localT) :: Text)
    values <- sequence
      [ toGValue (Just (show i))
      , toGValue (Just entryTitle)
      , toGValue =<< _Just showTime entrySeen
      , toGValue =<< _Just showTime entryAck
      , toGValue =<< _Just showTime entryDeadline
      , toGValue =<< _Just showTime entryDone
      ]
    #set entriesModel row [0..(fromIntegral (length values) - 1)] values

-- | Bind a command execution handler.
bindCommandHandler
  :: MonadUnliftIO m
  => Gui -> (Text -> m ()) -> m ()
bindCommandHandler Gui{..} callback = void $ do
  runInIO <- askRunInIO
  commandInput `on` #activate $ do
    text <- commandInput `get` #text
    runInIO $ callback text

-- | Clear the command input.
clearCommandInput :: MonadIO m => Gui -> m ()
clearCommandInput Gui{..} = do
  set commandInput [ #text := "" ]

-- | Show a popup with an error message.
showErrorMessage :: MonadIO m => Text -> m ()
showErrorMessage msg = void $ do
  dialog <- new Gtk.MessageDialog
    [ #text    := "Error:\n\n" <> msg
    , #buttons := Gtk.ButtonsTypeOk ]
  #run dialog
  #destroy dialog

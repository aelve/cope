{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}


module Cope.Main
(
  main,
)
where


import BasePrelude hiding (on)
import qualified Data.Text.All as T
import Data.Text.All (Text)
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import Data.GI.Base


main :: IO ()
main = do
  -- Create a window
  Gtk.init Nothing
  win <- new Gtk.Window
    [ #title          := "Cope"
    , #gravity        := Gdk.GravityCenter
    , #windowPosition := Gtk.WindowPositionCenter
    , #defaultWidth   := 600
    , #defaultHeight  := 500 ]
  win `on` #destroy $
    Gtk.mainQuit
  win `on` #keyPressEvent $ \e -> do
    keyVal  <- Gdk.getEventKeyKeyval e
    keyName <- Gdk.keyvalName keyVal
    if keyName == Just "Escape"
      then #destroy win >> pure True
      else pure False

  -- Create a text entry
  searchEntry <- new Gtk.Entry []

  -- Create a new list model
  model <- new Gtk.ListStore []
  view  <- new Gtk.TreeView
    [ #model          := model
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
        #appendColumn view column
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
  #add viewScrolled view

  -- Create a layout
  layout <- new Gtk.Table
    [ #nRows    := 2
    , #nColumns := 1 ]
  #attach layout searchEntry 0 1 0 1
    [Gtk.AttachOptionsFill] [Gtk.AttachOptionsFill] 0 0
  #attachDefaults layout viewScrolled 0 1 1 2
  #add win layout

  -- Start the program
  #showAll win
  Gtk.main

module Cope.Main
(
  main,
)
where


import Imports

import Cope.Types
import Cope.Gui


main :: IO ()
main = do
  gui <- createGui
  t <- getCurrentTime
  setEntries gui [Entry "hi" (Just t) Nothing Nothing Nothing]
  runGui gui

{-
next steps:
  command execution
  binding command execution to the field
-}


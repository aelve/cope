module Cope.Main
(
  main,
)
where


import Imports

import Cope.Gui


main :: IO ()
main = do
  gui <- createGui
  runGui gui

{-
next steps:
  command execution
  binding command execution to the field
-}


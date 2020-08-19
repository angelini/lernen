module Main where

import Lib
import View

main :: IO ()
main = do
  pairs <- parseFile "data/translations.xml"
  tui $ head pairs

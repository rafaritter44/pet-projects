module Main where

import Stockfischer.Game
import Stockfischer.Board

main :: IO ()
main = do
  playGame Human Computer White initialBoard

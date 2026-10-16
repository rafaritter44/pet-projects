module Main where

import Stockfischer.Game
import Stockfischer.Board

main :: IO ()
main = do
  startGame Human Computer

module Main where

import Stockfischer.Game

main :: IO ()
main = do
  startGame (Human, Computer)

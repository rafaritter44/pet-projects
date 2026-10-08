module Main where

import Game
import Board

main :: IO ()
main = do
  let getPieces = map (getSquare initialBoard) . getPiecePositions initialBoard
  print $ getPieces Black
  print $ getPieces White
  gameLoop initialBoard White

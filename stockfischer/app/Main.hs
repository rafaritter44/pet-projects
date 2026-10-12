module Main where

import Stockfischer.Game
import Stockfischer.Board

main :: IO ()
main = do
  let getPieces = map (getSquare initialBoard) . getPiecePositions initialBoard
  print $ getPieces Black
  print $ getPieces White
  gameLoop initialBoard White

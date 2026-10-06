module Main where

import Game
import Board
import qualified Data.Set as S

main :: IO ()
main = do
  let pieces color = map (getSquare initialBoard) (S.toList $ getPiecePositions initialBoard color)
  print $ pieces Black
  print $ pieces White
  gameLoop initialBoard White

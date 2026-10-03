module Board where

import Data.Matrix
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S

data Color = White | Black
  deriving (Eq)
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
data Piece = Piece Color PieceType
data Square = Empty | Occupied Piece

type Board = Matrix Square
type Position = (Int, Int)
type Move = (Position, Position)

instance Show Piece where
  show (Piece White King)   = "♔"
  show (Piece White Queen)  = "♕"
  show (Piece White Rook)   = "♖"
  show (Piece White Bishop) = "♗"
  show (Piece White Knight) = "♘"
  show (Piece White Pawn)   = "♙"
  show (Piece Black King)   = "♚"
  show (Piece Black Queen)  = "♛"
  show (Piece Black Rook)   = "♜"
  show (Piece Black Bishop) = "♝"
  show (Piece Black Knight) = "♞"
  show (Piece Black Pawn)   = "♟"

instance Show Square where
  show Empty        = "."
  show (Occupied p) = show p

boardString :: Color -> Board -> String
boardString White board =
  unlines $
  "  A B C D E F G H" :
  [ show row ++ " "
  ++ unwords [show (board ! (row, col)) | col <- [1..8]]
  ++ " " ++ show row
  | row <- [8,7..1]
  ]
  ++ ["  A B C D E F G H"]
boardString Black board =
  unlines $
  "  H G F E D C B A" :
  [ show row ++ " "
  ++ unwords [show (board ! (row, col)) | col <- [8,7..1]]
  ++ " " ++ show row
  | row <- [1..8]
  ]
  ++ ["  H G F E D C B A"]

parseMove :: String -> Maybe Move
parseMove [fromFile, fromRank, toFile, toRank] = do
  from <- parsePosition fromFile fromRank
  to   <- parsePosition toFile toRank
  return (from, to)
parseMove _ = Nothing

parsePosition :: Char -> Char -> Maybe Position
parsePosition file rank = do
  col <- fileToCol file
  row <- rankToRow rank
  return (row, col)

fileToCol :: Char -> Maybe Int
fileToCol file =
  case toUpper file of
    'A' -> Just 1
    'B' -> Just 2
    'C' -> Just 3
    'D' -> Just 4
    'E' -> Just 5
    'F' -> Just 6
    'G' -> Just 7
    'H' -> Just 8
    _   -> Nothing

rankToRow :: Char -> Maybe Int
rankToRow '1' = Just 1
rankToRow '2' = Just 2
rankToRow '3' = Just 3
rankToRow '4' = Just 4
rankToRow '5' = Just 5
rankToRow '6' = Just 6
rankToRow '7' = Just 7
rankToRow '8' = Just 8
rankToRow _   = Nothing

initialBoard :: Board
initialBoard = fromList 8 8 squares
  where squares =
          pieces White
          ++ pawns White
          ++ replicate 32 Empty
          ++ pawns Black
          ++ pieces Black
        pieces color =
          map (Occupied . Piece color) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
        pawns color =
          replicate 8 $ Occupied $ Piece color Pawn

nextLegalBoards :: Board -> Color -> M.Map Move Board
nextLegalBoards _ _ = M.empty

piecePositions :: Board -> Color -> S.Set Position
piecePositions board color =
  S.fromList
  [ (row, col)
  | row <- [1 .. nrows board]
  , col <- [1 .. ncols board]
  , Occupied (Piece pieceColor _) <- [getElem row col board]
  , pieceColor == color
  ]

attackedPositions :: Board -> Color -> S.Set Position
attackedPositions _ _ = S.empty

attackedPositionsFrom :: Board -> Position -> S.Set Position
attackedPositionsFrom _ _ = S.empty

reachablePositionsFrom :: Board -> Position -> S.Set Position
reachablePositionsFrom _ _ = S.empty

inCheck :: Board -> Color -> Bool
inCheck _ _ = False

nextBoard :: Board -> Move -> Board
nextBoard board (from, to) =
  setElem Empty from $
  setElem (board ! from) to board

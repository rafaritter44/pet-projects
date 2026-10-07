module Board where

import Move
import Data.Matrix
import Data.List
import Data.Function
import qualified Data.Map as M
import qualified Data.Set as S

data Color = White | Black
  deriving (Eq, Show)
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
data Piece = Piece Color PieceType
data Square = Empty | Occupied Piece

newtype Board = Board (Matrix Square)

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

instance Show Board where
  show board = intercalate "\n" $
    "  A B C D E F G H  " :
    [ show row ++ " "
      ++ unwords [show (getSquare board (Position row col)) | col <- [1..8]]
      ++ " " ++ show row
    | row <- [8,7..1]
    ]
    ++ ["  A B C D E F G H  "]

initialBoard :: Board
initialBoard = Board $ fromList 8 8 squares
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

getNextPlayer :: Color -> Color
getNextPlayer White = Black
getNextPlayer Black = White

getSquare :: Board -> Position -> Square
getSquare (Board board) (Position row col) = board ! (row, col)

setSquare :: Position -> Square -> Board -> Board
setSquare (Position row col) square (Board board) = Board $
  setElem square (row, col) board

getNextLegalBoards :: Board -> Color -> M.Map Move Board
getNextLegalBoards _ _ = M.empty

getPiecePositions :: Board -> Color -> S.Set Position
getPiecePositions board color = S.fromList
  [ Position row col
  | row <- [1..8]
  , col <- [1..8]
  , Occupied (Piece pieceColor _) <- [getSquare board (Position row col)]
  , pieceColor == color
  ]

getAttackedPositions :: Board -> Color -> S.Set Position
getAttackedPositions _ _ = S.empty

getAttackedPositionsFrom :: Board -> Position -> S.Set Position
getAttackedPositionsFrom _ _ = S.empty

getReachablePositionsFrom :: Board -> Position -> S.Set Position
getReachablePositionsFrom _ _ = S.empty

isInCheck :: Board -> Color -> Bool
isInCheck _ _ = False

getNextBoard :: Board -> Move -> Board
getNextBoard board (Move from to) = board
  & setSquare from Empty
  & setSquare to (getSquare board from)

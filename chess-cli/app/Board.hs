module Board where

import Data.Matrix
import Data.Set (Set, empty)

data Color = White | Black
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

initialBoard :: Board
initialBoard = fromList 8 8 squares
  where squares =
          pieces Black
          ++ pawns Black
          ++ replicate 32 Empty
          ++ pawns White
          ++ pieces White
        pieces color =
          map (Occupied . Piece color) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
        pawns color =
          replicate 8 $ Occupied $ Piece color Pawn

legalMoves :: Board -> Color -> Set Move
legalMoves _ _ = empty

piecePositions :: Board -> Color -> Set Position
piecePositions _ _ = empty

legalMovesFrom :: Board -> Position -> Set Move
legalMovesFrom _ _ = empty

module Board where

import Move
import Data.Matrix
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

data Color = White | Black
  deriving (Eq)
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
  show (Board board) = intercalate "\n" $
    "  A B C D E F G H  " :
    [ show row ++ " "
      ++ unwords [show (board ! (row, col)) | col <- [1..8]]
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

getSquare :: Board -> Position -> Square
getSquare (Board board) position = board ! position

setSquare :: Board -> Position -> Square -> Board
setSquare (Board board) position square = Board $
  setElem square position board

nextLegalBoards :: Board -> Color -> M.Map Move Board
nextLegalBoards _ _ = M.empty

piecePositions :: Board -> Color -> S.Set Position
piecePositions board color = S.fromList
  [ (row, col)
  | row <- [1..8]
  , col <- [1..8]
  , Occupied (Piece pieceColor _) <- [getSquare board (row, col)]
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
  setSquare (setSquare board from Empty) to $ getSquare board from

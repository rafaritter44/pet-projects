module Stockfischer.Board where

import Stockfischer.Moves
import Data.Matrix
import Data.List
import Data.Function

data Color = White | Black
  deriving (Eq, Show)
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq)
data Piece = Piece Color PieceType
  deriving (Eq)
data Square = Empty | Occupied Piece
  deriving (Eq)

newtype Board = Board (Matrix Square)
  deriving (Eq)

type Game = [Board]

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
  show Empty        = "·"
  show (Occupied p) = show p

instance Show Board where
  show board = intercalate "\n" $ header : rows ++ [header]
    where
      header =  "  a b c d e f g h  "
      rows   = [ show row ++ " "
                 ++ unwords [show $ getSquare board $ Position row col | col <- [1..8]]
                 ++ " " ++ show row
               | row <- [8,7..1]
               ]

initialBoard :: Board
initialBoard = Board $ fromList 8 8 squares
  where
    squares =
      getPieces White
      ++ getPawns White
      ++ replicate 32 Empty
      ++ getPawns Black
      ++ getPieces Black
    getPieces color =
      map (Occupied . Piece color) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
    getPawns color =
      replicate 8 $ Occupied $ Piece color Pawn

isInitialPosition :: Position -> Square -> Bool
isInitialPosition = (==) . getSquare initialBoard

getSquare :: Board -> Position -> Square
getSquare (Board board) (Position row col) = getElem row col board

setSquare :: Position -> Square -> Board -> Board
setSquare (Position row col) square (Board board) = Board $
  setElem square (row, col) board

getKingPosition :: Board -> Color -> Position
getKingPosition board player = head
  [ position
  | position <- getPiecePositions board player
  , getSquare board position == Occupied (Piece player King)
  ]

getPiecePositions :: Board -> Color -> [Position]
getPiecePositions board player =
  [ position
  | position <- getAllPiecePositions board
  , Occupied (Piece color _) <- [getSquare board position]
  , color == player
  ]

getAllPiecePositions :: Board -> [Position]
getAllPiecePositions board =
  [ Position row col
  | row <- [1..8]
  , col <- [1..8]
  , Occupied _ <- [getSquare board (Position row col)]
  ]

getNextBoard :: Board -> Move -> Board
getNextBoard board (Move from to) = board
  & setSquare from Empty
  & setSquare to (getSquare board from)

getCurrentBoard :: Game -> Board
getCurrentBoard = head

getPreviousBoard :: Game -> Maybe Board
getPreviousBoard (_:board:_) = Just board
getPreviousBoard _           = Nothing

getCurrentPlayer :: Game -> Color
getCurrentPlayer game
  | odd $ length game = White
  | otherwise         = Black

getNextPlayer :: Color -> Color
getNextPlayer White = Black
getNextPlayer Black = White

isCaptureOrPawnMove :: Board -> Board -> Bool
isCaptureOrPawnMove currentBoard previousBoard =
  countPieces currentBoard < countPieces previousBoard
  || getPawnPositions currentBoard /= getPawnPositions previousBoard
  where
    countPieces = length . getAllPiecePositions
    getPawnPositions board =
      [ position
      | position <- getAllPiecePositions board
      , Occupied (Piece _ Pawn) <- [getSquare board position]
      ]

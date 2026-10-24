module Stockfischer.Board where

import qualified Stockfischer.Moves as M
import Stockfischer.Moves hiding (Queen, Rook, Bishop, Knight)
import Stockfischer.Players
import Data.Matrix
import Data.List
import Data.Function

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

isPathClear :: Board -> Position -> Position -> Bool
isPathClear board from to =
  all ((== Empty) . getSquare board) $ getPositionsBetween from to

playMove :: Board -> Move -> Board
playMove board move@(Move from to) = case getSquare board from of
  Occupied (Piece color King)
    | Just castling <- getCastling color from to -> board
      & movePiece move
      & movePiece (getRookCastlingMove castling)
  _ -> movePiece move board
playMove board move@(Promotion {}) = movePiece move board

movePiece :: Move -> Board -> Board
movePiece (Move from to) board = board
  & setSquare from Empty
  & setSquare to (getSquare board from)
movePiece (Promotion from to piece) board = case getSquare board from of
  Occupied (Piece color Pawn) -> board
    & setSquare from Empty
    & setSquare to (Occupied $ Piece color $ getPromotionPiece piece)
  _ -> board

getPromotionPiece :: M.PromotionPiece -> PieceType
getPromotionPiece M.Queen  = Queen
getPromotionPiece M.Rook   = Rook
getPromotionPiece M.Bishop = Bishop
getPromotionPiece M.Knight = Knight

getCurrentBoard :: Game -> Board
getCurrentBoard = head

getPreviousBoard :: Game -> Maybe Board
getPreviousBoard (_:board:_) = Just board
getPreviousBoard _           = Nothing

getCurrentPlayer :: Game -> Color
getCurrentPlayer game
  | odd $ length game = White
  | otherwise         = Black

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

hasEverMoved :: Game -> Position -> Bool
hasEverMoved (currentBoard:previousBoard:previousBoards) position =
  hasMoved currentBoard previousBoard position
  || hasEverMoved (previousBoard:previousBoards) position
hasEverMoved _ _ = False

hasMoved :: Board -> Board -> Position -> Bool
hasMoved currentBoard previousBoard position =
  getSquare currentBoard position /= getSquare previousBoard position

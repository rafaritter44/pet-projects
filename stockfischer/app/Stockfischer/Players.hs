module Stockfischer.Players where

data Color = White | Black
  deriving (Eq, Show)

data PlayerType = Human | Computer

type PlayerTypes = (PlayerType, PlayerType)

getNextPlayer :: Color -> Color
getNextPlayer White = Black
getNextPlayer Black = White

getBoardPerspective :: PlayerTypes -> Color -> Color
getBoardPerspective (Human   , Human)    player = player
getBoardPerspective (Human   , Computer) _      = White
getBoardPerspective (Computer, Human)    _      = Black
getBoardPerspective (Computer, Computer) _      = White

getPlayerType :: Color -> PlayerTypes -> PlayerType
getPlayerType White (whitePlayerType, _) = whitePlayerType
getPlayerType Black (_, blackPlayerType) = blackPlayerType

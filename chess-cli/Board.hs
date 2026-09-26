data Color = White | Black
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
data Piece = Piece PieceType Color
data Square = Empty | Occupied Piece

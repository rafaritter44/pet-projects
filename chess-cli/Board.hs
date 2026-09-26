data Color = White | Black
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
data Piece = Piece Color PieceType
data Square = Empty | Occupied Piece

pieceUnicode :: Piece -> Char
pieceUnicode (Piece White King)   = '♔'
pieceUnicode (Piece White Queen)  = '♕'
pieceUnicode (Piece White Rook)   = '♖'
pieceUnicode (Piece White Bishop) = '♗'
pieceUnicode (Piece White Knight) = '♘'
pieceUnicode (Piece White Pawn)   = '♙'
pieceUnicode (Piece Black King)   = '♚'
pieceUnicode (Piece Black Queen)  = '♛'
pieceUnicode (Piece Black Rook)   = '♜'
pieceUnicode (Piece Black Bishop) = '♝'
pieceUnicode (Piece Black Knight) = '♞'
pieceUnicode (Piece Black Pawn)   = '♟'

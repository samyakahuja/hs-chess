module Board where

import Data.Array

-- | Types

type Pos = (Int, Int)
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Read, Eq, Ord)
data PieceSide = Black | White deriving (Eq)

data Piece = Piece {
    pieceSide :: PieceSide,
    pieceType :: PieceType,
    piecePos  :: Pos
} deriving (Show, Eq)

type Square = Maybe Piece
type Board  = Array Pos Square
type Move   = (Piece, Pos)

-- | Show

instance Show PieceSide where
    show Black = "B"
    show White = "W"

instance Show PieceType where
    show Pawn   = "P"
    show Knight = "N"
    show Bishop = "B"
    show Rook   = "R"
    show Queen  = "Q"
    show King   = "K"

prettyBoard :: Board -> [String]
prettyBoard b = [[ prettySquare $ b ! (y,x) | y <- [1..8] ] | x <- [1..8] ]

prettySquare :: Square -> Char
prettySquare Nothing  = '.'
prettySquare (Just p) = icon p

printBoard :: Board -> IO ()
printBoard b = do
    putStrLn $ "  " ++ (unwords $ space ['a'..'h'])
    mapM_ putStrLn $ zipWith (++) (space ['8','7'..]) $ (map (unwords . space) (prettyBoard b))
    where
        space = map (: " ")

icon :: Piece -> Char
icon (Piece s t _) =
    case s of
        Black -> fst
        White -> snd
    $
    case t of
        Pawn   -> ('♙','♟')
        Knight -> ('♘','♞')
        Bishop -> ('♗','♝')
        Rook   -> ('♖','♜')
        Queen  -> ('♕','♛')
        King   -> ('♔','♚')

-- | Functions

boardUpdate :: Board -> [Piece] -> Board
boardUpdate b ps = b // [ (piecePos p, Just p) | p <- ps ]

-- | Boards

blankBoard :: Board
blankBoard = listArray ((1,1),(8,8)) $ repeat Nothing

initialBoard :: Board
initialBoard = boardUpdate blankBoard ps where
    ps = [Piece Black Pawn (x,2) | x <- [1..8]]
      ++ [Piece White Pawn (x,7) | x <- [1..8]]
      ++ zipWith (Piece Black) row [(i,1) | i <- [1..8]]
      ++ zipWith (Piece White) row [(i,8) | i <- [1..8]]
    row = [Rook,Knight,Bishop,Queen,King,Bishop,Knight,Rook]


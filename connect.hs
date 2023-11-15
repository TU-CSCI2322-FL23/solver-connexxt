import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ratio ((%))
import Data.Tuple (swap)
import Debug.Trace

data Color = Red | Black deriving (Eq, Show)

type Player = (String, Color) -- instance of the person playing (Name, Red or Black)
data Winner = Tie | Win Color
--type Winner = Maybe Color -- red, black, or null
type Board = [[Maybe Color]]-- write a list of lists laterrr
type Game = (Board, Color)

type Move = Int -- what's the index into the column?
-- in connect 4, you can only choose the x coordinate
-- so you can only look @ which List you're affecting

-- Data Winner = Tie | Won Color deriving (Eq, Show) --You canot share constructors, for instance between Color and Winner. The solution is to have Winner *store* a color in one of the constructors


-- whosTurn :: Player -> Bool
-- whosTurn person =

-- MAKE FUNCTION TO CATCH USER ERRORS FOR MAKING PLAYERS (MORE THAN 1 OR LESS THAN 2)

makeMove :: Game -> Move -> Game
makeMove (board, playerColor) column
    | isValidMove board column = (updatedBoard, nextPlayerColor playerColor)
    | otherwise = error "Invalid Move"
    where
        updatedBoard = dropPiece board column playerColor
        dropPiece :: Board -> Move -> Color -> Board
        dropPiece [] _ _ = []
        dropPiece (c:cols) 0 color = placePiece c cols : c
        dropPiece (c: cols) n color = c : dropPiece cols (n-1) color
        placePiece :: [Maybe Color] -> Color -> [Maybe Color]
        placePiece [] _ = []
        placePiece (Nothing:rest) color = Just color: rest
        placePiece (piece:rest) _ = piece : placePiece rest 
        nextPlayerColor Red = Black
        nextPlayerColor Black = Red


isValidMove :: Board -> Move -> Bool
isValidMove board column 
    | column < 0 || column >= length (head board) = False
    | all isJust (getColumn board column) = False
    | otherwise = True
    where 
        getColumn :: Board -> Move -> [Maybe Color]
        getColumn [] _ = []
        getColumn (c:cols) 0 = c
        getColumn (c: cols) n = getColumn cols (n-1)

-- isValidMove :: Move -> Bool
-- isValidMove move = 


lstValidMoves :: Board -> [Move]
lstValidMoves board = [col | col <- [0..(length(head board)-1)], isValidMove board col]


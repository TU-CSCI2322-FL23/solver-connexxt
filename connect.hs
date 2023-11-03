import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ratio ((%))
import Data.Tuple (swap)
import Debug.Trace

data Color = Red | Black

type Player = (String, Color) -- instance of the person playing (Name, Red or Black)

type Board = [[Maybe Color]] -- write a list of lists laterrr

type Game = (Board, Color)

type Move = Int -- what's the index into the column?
-- in connect 4, you can only choose the x coordinate
-- so you can only look @ which List you're affecting

-- Data Winner = Tie | Won Color --You canot share constructors, for instance between Color and Winner. The solution is to have Winner *store* a color in one of the constructors

-- whosTurn :: Player -> Bool
-- whosTurn person =

-- MAKE FUNCTION TO CATCH USER ERRORS FOR MAKING PLAYERS (MORE THAN 1 OR LESS THAN 2)

-- type Game = Location | Turn

-- makeMove :: Move -> Board
-- makeMove move =

-- isValidMove :: Move -> Bool
-- isValidMove move = 

-- lstValidMoves :: Board -> [Move]
-- lstValidMoves board = 
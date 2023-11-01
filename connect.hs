import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ratio ((%))
import Data.Tuple (swap)
import Debug.Trace

data Color = Red | Black 
type Player = (String, Color) -- instance of the person playing (Name, Red or Black)
data Winner = Tie | Red | Black | Null 
--type Winner = Maybe Color -- red, black, or null
type Board = [[Maybe Color]]-- write a list of lists laterrr
type Game = (Board, Color, Winner)
type Move = Int -- what's the index into the column?
-- in connect 4, you can only choose the x coordinate
--so you can only look @ which List you're affecting 


--MAKE FUNCTION TO CATCH USER ERRORS FOR MAKING PLAYERS (MORE THAN 1 OR LESS THAN 2)

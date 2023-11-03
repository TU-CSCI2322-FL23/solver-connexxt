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

--MAKE FUNCTION TO CATCH USER ERRORS FOR MAKING PLAYERS (MORE THAN 1 OR LESS THAN 2)
checkWin:: Board -> Maybe Winner
checkWin currentBoard = 
    if (checkVertical(currentBoard) != null) then  checkVertical currentBoard
    else if (checkHorizontal(currentBoard) != null) then  checkHorizontal currentBoard
    else if (checkDiagonal(currentBoard) != null) then  checkDiagonal currentBoard
    else  null

 checkVertical:: Board -> Maybe Winner
 checkVertical currentBoard= findWin[checkFour x | x <- currentBoard]   -- this makes a list of the first element of every list
     
  
    --for( i <- 1-4; i ++){ -- checks the four win cases (Maybe make this a helper function)
    --    if (fst elements == snd elements) -- if first two match
        
    --}
 
checkHorizontal:: Board -> Maybe Winner
 checkHorizontal currentBoard= undefined -- instead of taking one column and going four deep; take 4 columns and check them in lock step 

    --FILL OUT HERE
 
checkDiagonal:: Board -> Maybe Winner
checkDiagonal currentBoard= undefined
    --FILL OUT HERE
 
checkFour::[Maybe Color] -> Maybe Color
checkFour (Just Red: Just Red:Just Red:Just Red: _) = Just Red
checkFour (Just Black: Just Black:Just Black:Just Black: _) = Just Black
checkFour (w: rest) = checkFour rest 
checkFour lst = Nothing
 
findWin::[Maybe Color]-> Maybe Color
--pattern through list, if color return otherwise Nothing
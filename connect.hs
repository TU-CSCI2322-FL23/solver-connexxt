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
checkWin:: Board -> Maybe Winner  --might want to change input to game type later
checkWin currentBoard = 
     (checkVertical currentBoard) `combineChecks` (checkHorizontal currentBoard ) `combineChecks` (checkDiagonal currentBoard) -- Dr Fogarty Approved !

combineChecks:: Maybe a -> Maybe a -> Maybe a --Dr Fogarty Approved
combineChecks (Just x) _ = Just x
combineChecks Nothing (Just xx) = Just xx
combineChecks Nothing Nothing= Nothing

 checkVertical:: Board -> Maybe Winner
 checkVertical currentBoard = findWin[checkFour x | x <- currentBoard]   -- Dr Fogarty Approved!
 
checkHorizontal:: Board -> Maybe Winner
checkHorizontal (one : two : three : four : rest)=
    findWin[checkFourAcross one two three four]  -- NOT DONE
    --findWin[ head column | column <- currentBoard] -- if I don't make a new helper 
    --checkFour y | y <- currentBoard, y == fst currentBoard] 
-- list of head colfumns where columkn comes from board
-- instead of taking one column and going four deep; take 4 columns and check them in lock step 

    --FILL OUT HERE
 
checkDiagonal:: Board -> Maybe Winner
checkDiagonal currentBoard= 
    checkFourAcross (head currentBoard) (head (drop 1 currentBoard)) (head (drop 2 currentBoard)) ( head (drop 3 currentBoard))
    --FILL OUT HERE
    -- so like using check four across but dropping the first one from the second and the first two from the third and then the firsth three from the fourth column and then doing that all resurvibely
 
checkFour::[Maybe Color] -> Maybe Color --Dr Fogarty Approved!
checkFour (Just Red: Just Red:Just Red:Just Red: _) = Just Red
checkFour (Just Black: Just Black:Just Black:Just Black: _) = Just Black
checkFour (w: rest) = checkFour rest 
checkFour lst = Nothing

checkFourAcross:: [Maybe Color] -> [Maybe Color] -> [Maybe Color] -> [Maybe Color] -> Maybe Color
checkFourAcross (Just Red: _ ) (Just Red: _ )(Just Red: _ )(Just Red: _ ) = Just Red
checkFourAcross (Just Black: _ )(Just Black: _ )(Just Black: _ )(Just Black: _ ) = Just Black
checkFourAcross (c: a )(c: b )(c: y )(c: d ) = checkFourAcross a b y d
checkFourAcross z y x a = Nothing 

-- check four across where I would take in 4 clukmns and I pattern match the head
-- so like (red: _) (red: _) (red: _)
 
findWin::[Maybe Color]-> Maybe Color
--pattern through list, if color return otherwise Nothing
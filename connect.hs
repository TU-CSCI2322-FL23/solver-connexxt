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

--MAKE FUNCTION TO CATCH USER ERRORS FOR MAKING PLAYERS (MORE THAN 1 OR LESS THAN 2)
checkWin:: Game -> Maybe Winner  --might want to change input to game type later
checkWin currentBoard = 
     (checkVertical currentBoard) `combineChecks` (checkHorizontal currentBoard ) `combineChecks` (checkDiagonal currentBoard) -- Dr Fogarty Approved !

combineChecks:: Maybe a -> Maybe a -> Maybe a --Dr Fogarty Approved
combineChecks (Just x) _ = Just x
combineChecks Nothing (Just xx) = Just xx
combineChecks Nothing Nothing= Nothing

checkVertical:: Board -> Maybe Winner
checkVertical currentBoard = findWin[checkFour x | x <- currentBoard]   -- Dr Fogarty Approved!
 
checkHorizontal:: Board -> Maybe Winner -- Dr Fogarty Approved!
checkHorizontal [one , two , three , four , five, six, seven]=
    let 
        firstFour = checkFourAcross one two three four
        middleFour = checkFourAcross two three four five
        almostEndFour = checkFourAcross three four five six
        endFour = checkFourAcross four five six seven
    in findWin[firstFour, middleFour, almostEndFour, endFour] 
checkHorizontal lst = error " why are the dimensions of your board weird?" -- Dr Fogarty Approved!
    --findWin[ head column | column <- currentBoard] -- if I don't make a new helper 
    --checkFour y | y <- currentBoard, y == fst currentBoard] 
-- list of head colfumns where columkn comes from board
-- instead of taking one column and going four deep; take 4 columns and check them in lock step 

    --FILL OUT HERE
 
checkDiagonal:: Board -> Maybe Color --Dr Fogarty Approved
checkDiagonal [col1, col2, col3]  = Nothing
checkDiagonal (col1: col2: col3 : col4 : rest)= 
    let beginning = checkFourAcross col1 (drop 1 col2) (drop 2 col3) (drop 3 col4)
        later = checkDiagonal (col2 : col3 : col4 : rest)
   -- checkFourAcross ( drop 1 col1) (drop 2 col2) (drop 3 col3) (drop 4 col4)
  --  checkFourAcross (drop 2 col1) (drop 3 col2) (drop 4 col3) (drop 5 col4)
    --checkFourAcross (drop 3 col1) (drop 4 col2) (drop 5 col3) (drop 6 col4)
    in findWin [beginning, later]
checkDiagonal lst = error "Incorrect Board Dimensions!"
    
    --FILL OUT HERE
    -- so like using check four across but dropping the first one from the second and the first two from the third and then the firsth three from the fourth column and then doing that all resurvibely
 
checkFour::[Maybe Color] -> Maybe Color --Dr Fogarty Approved!
checkFour (Just Red: Just Red:Just Red:Just Red: _) = Just Red
checkFour (Just Black: Just Black:Just Black:Just Black: _) = Just Black
checkFour (w: rest) = checkFour rest 
checkFour lst = Nothing

checkFourAcross:: [Maybe Color] -> [Maybe Color] -> [Maybe Color] -> [Maybe Color] -> Maybe Color -- Dr Fogarty Approved!
checkFourAcross (Just Red: _ ) (Just Red: _ )(Just Red: _ )(Just Red: _ ) = Just Red
checkFourAcross (Just Black: _ )(Just Black: _ )(Just Black: _ )(Just Black: _ ) = Just Black
checkFourAcross (c: a )(c: b )(c: y )(c: d ) = checkFourAcross a b y d
checkFourAcross z y x a = Nothing 

-- check four across where I would take in 4 clukmns and I pattern match the head
-- so like (red: _) (red: _) (red: _)
 
findWin::[Maybe Color]-> Maybe Color --Dr Fogarty Approved
findWin potentialWins = 
    if  if Just Red `elem` potentialWins  && Just Black `elem` potentialWins then error "multiple winners"
    else if Just Red `elem` potentialWins then Just Red
    else if Just Black `elem` potentialWins then Just Black
    else Nothing 
-- careful ab preferenc e for Just Red, might be issue in sprint 2
--findWind [Just Red: Just Red : Just Red : Just Red ] = Just Red
--findWind [Just Black : Just Black : Just Black : Just Black ] = Just Black
--findWind [_ : _ : _ : _ ] = Nothing 
--pattern through list, if color return otherwise Nothing

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



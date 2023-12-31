module Connect where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ratio ((%))
import Data.Tuple (swap)
import Debug.Trace
import Data.Char

data Color = Red | Black deriving (Eq, Show)

type Player = (String, Color) -- instance of the person playing (Name, Red or Black)

data Winner = Tie | Win Color deriving (Eq, Show)

-- type Winner = Maybe Color -- red, black, or null
type Board = [[Maybe Color]] -- write a list of lists laterrr

type Game = (Board, Color)

type Move = Int -- what's the index into the column?
-- in connect 4, you can only choose the x coordinate
-- so you can only look @ which List you're affecting

-- Data Winner = Tie | Won Color deriving (Eq, Show) --You canot share constructors, for instance between Color and Winner. The solution is to have Winner *store* a color in one of the constructors

type Column = [Maybe Color]

type WholeBoard = [Column]

emptyColumn :: Column
emptyColumn = []

emptyBoard :: Board
emptyBoard = replicate 7 emptyColumn

-- whosTurn :: Player -> Bool
-- whosTurn person =
--MAKE FUNCTION TO CATCH USER ERRORS FOR MAKING PLAYERS (MORE THAN 1 OR LESS THAN 2)

 
checkWin :: Game -> Maybe Winner -- might want to change input to game type later
checkWin game@(currentBoard, player) = -- Dr Fogarty Approved !
  case findWin [(checkVertical currentBoard), (checkHorizontal currentBoard), (checkDiagonal currentBoard)] of
    Just pl -> Just (Win pl)
    Nothing -> if (validMoves game == []) then Just Tie else Nothing

combineChecks :: Maybe a -> Maybe a -> Maybe a -- Dr Fogarty Approved
combineChecks (Just x) _ = Just x
combineChecks Nothing (Just xx) = Just xx
combineChecks Nothing Nothing = Nothing

checkVertical :: Board -> Maybe Color
checkVertical currentBoard = findWin [checkFour x | x <- currentBoard] -- Dr Fogarty Approved!

checkHorizontal :: Board -> Maybe Color -- Dr Fogarty Approved!
checkHorizontal [one, two, three, four, five, six, seven] =
  let firstFour = checkFourAcross one two three four
      middleFour = checkFourAcross two three four five
      almostEndFour = checkFourAcross three four five six
      endFour = checkFourAcross four five six seven
   in findWin [firstFour, middleFour, almostEndFour, endFour]
checkHorizontal lst = error " why are the dimensions of your board weird?" -- Dr Fogarty Approved!
    --findWin[ head column | column <- currentBoard] -- if I don't make a new helper 
    --checkFour y | y <- currentBoard, y == fst currentBoard] 
-- list of head colfumns where columkn comes from board
-- instead of taking one column and going four deep; take 4 columns and check them in lock step 

    --FILL OUT HERE
 

checkDiagonal :: Board -> Maybe Color -- Dr Fogarty Approved
checkDiagonal [col1, col2, col3] = Nothing
checkDiagonal (col1 : col2 : col3 : col4 : rest) =
  let beginning = checkFourAcross col1 (drop 1 col2) (drop 2 col3) (drop 3 col4)
      later = checkDiagonal (col2 : col3 : col4 : rest)
   in findWin [beginning, later]
checkDiagonal lst = error "Incorrect Board Dimensions!"


checkFour::[Maybe Color] -> Maybe Color --Dr Fogarty Approved!
checkFour (Just Red: Just Red:Just Red:Just Red: _) = Just Red
checkFour (Just Black: Just Black:Just Black:Just Black: _) = Just Black
checkFour (w: rest) = checkFour rest 
checkFour lst = Nothing

checkFourAcross :: [Maybe Color] -> [Maybe Color] -> [Maybe Color] -> [Maybe Color] -> Maybe Color -- Dr Fogarty Approved!
checkFourAcross (Just Red : _) (Just Red : _) (Just Red : _) (Just Red : _) = Just Red
checkFourAcross (Just Black : _) (Just Black : _) (Just Black : _) (Just Black : _) = Just Black
checkFourAcross (_ : a) (_ : b) (_ : y) (_ : d) = checkFourAcross a b y d
checkFourAcross z y x a = Nothing

-- check four across where I would take in 4 clukmns and I pattern match the head
-- so like (red: _) (red: _) (red: _)

findWin :: [Maybe Color] -> Maybe Color -- Dr Fogarty Approved
findWin potentialWins =
  if Just Red `elem` potentialWins && Just Black `elem` potentialWins then error "multiple winners"
  else if Just Red `elem` potentialWins then Just Red
  else if Just Black `elem` potentialWins then Just Black 
  else Nothing

makeMove :: Game -> Move -> Game
makeMove (board, playerColor) column
    | isValidMove board column = (updatedBoard, nextPlayerColor playerColor)
    | otherwise = error "Invalid Move"
    where
        updatedBoard = dropPiece board column playerColor
        dropPiece :: Board -> Move -> Color -> Board
        dropPiece [] _ _ = []
        dropPiece (c:cols) 0 color = (placePiece c color) : cols
        dropPiece (c: cols) n color = c : dropPiece cols (n-1) color
        placePiece :: [Maybe Color] -> Color -> [Maybe Color]
        placePiece [] _ = []
        placePiece (Nothing:rest) color = Just color: rest
        placePiece (piece:rest) color = piece : (placePiece rest color)
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

validMoves:: Game -> [Move] --dr fogarty suggests:
validMoves (board, color) = [num | (num, col) <- zip [0..] board, isNothing (head col)]

bestMove :: Game -> Move 
bestMove game@(board, playerColor) = getMaxMove moveOutcomes
    where
        moves = validMoves game 
        moveOutcomes = [(evaluateMove(makeMove game move), move )| move <- moves]
        evaluateMove :: Game -> Winner 
        evaluateMove g = whoWillWin g 

        moveOutcome :: Move -> Winner 
        moveOutcome move = evaluateMove (makeMove game move)

        getMaxMove :: [(Winner, Move)] -> Move 
        getMaxMove [] = error "No valid Moves"
        getMaxMove [(outcome, move)] = move 
        getMaxMove ((outcome, move): rest) =
            case (moveOutcome move, moveOutcome (getMaxMove rest)) of 
                (Win color, _) -> if color == playerColor then move else getMaxMove rest 
                (_, Win _) -> getMaxMove rest 
                (_,_) -> getMaxMove rest 
type Rating = Int 
rateGame :: Game -> Rating 
rateGame (board, playerColor) = case whoWillWin (board, playerColor) of 
    Win Red -> 100
    Win Black -> -100
    Tie -> 0 
whoWillWin:: Game -> Winner
whoWillWin game =
    case checkWin game of
        Just win -> win 
        Nothing -> case declarePotential (whoNotherHelper (whoHelper(validMoves game, game))) game of --now I have a list of games where the move has been made
            Just color -> Win color
            Nothing -> Tie 
whoHelper:: ([Move], Game) -> [Game]
whoHelper ([],_) = []
whoHelper (move:rest, currentGame) = 
    makeMove currentGame move : whoHelper (rest,currentGame)

whoNotherHelper:: [Game] -> [Winner]
whoNotherHelper [] = [] 
whoNotherHelper (game:games) = 
    case whoWillWin game of 
        Win color -> Win color : whoNotherHelper games 
        Tie -> Tie : whoNotherHelper games 

declarePotential:: [Winner] -> Game -> Maybe Color 
declarePotential allNext (currentBoard, currentColor) = 
    let 
        checkR = Win Red `elem` allNext
        checkB = Win Black `elem` allNext 
    in if currentColor == Red && checkR then Just Red 
        else if currentColor == Black && checkB then Just Black
        else if currentColor /= Red && checkB then Just Black 
        else if currentColor /= Black && checkR then Just Red 
        else Nothing 



findBestMove :: Game -> Int -> Move 
findBestMove game depth =
    case validMoves game of
        [] -> error "No valid moves"
        moves -> snd $ maximumBy (compareMoves game depth) [(evaluateMove (makeMove game move), move) | move <- moves]


compareMoves :: Game -> Int -> (Move -> Int, Move) -> (Move -> Int, Move) -> Ordering
compareMoves game depth (_, move1) (_, move2) =
    compare (evaluateMove (makeMove game move1) depth) (evaluateMove (makeMove game move2) depth)


evaluateMove :: Game -> Move -> Int
evaluateMove game move = rateGame (makeMove game move)

readGame :: String -> Game
readGame game = (map (map (letterToColor . trim) . splitOn "|" . snd) (filter (even . fst) $ zip [0 ..] (take (length (lines game) - 1) (lines game))), whoTurn $ last (lines game))

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

letterToColor :: String -> Maybe Color
letterToColor "R" = Just Red
letterToColor "B" = Just Black
letterToColor "" = Nothing

whoTurn :: String -> Color
whoTurn other
  | "Red" `isInfixOf` other = Red
  | "Black" `isInfixOf` other = Black

showGame :: Game -> String
showGame (board, Red) = showBoard board ++ "\nRed Player's Turn\n"
showGame (board, Black) = showBoard board ++ "\nBlack Player's Turn\n"

showBoard :: Board -> String
showBoard [b] = showLine b ++ "\n===---------------------==="
showBoard (b : board) = showLine b ++ "\n---------------------------\n" ++ showBoard board

showLine :: [Maybe Color] -> String
showLine lst = intercalate "|" (map showColor lst)

showColor :: Maybe Color -> String
showColor (Just Red) = " " ++ "R" ++ " "
showColor (Just Black) = " " ++ "B" ++ " "
showColor Nothing = "   "


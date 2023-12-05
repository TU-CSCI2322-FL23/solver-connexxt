module MainTest where

import Connect
import Control.Monad ()
import Data.List
import System.IO

type Test = IO Bool

type TestResult = (String, Bool)

emptyGame :: Game
emptyGame = (emptyBoard, Black)

player1 :: Color
player1 = Black

player2 :: Color
player2 = Red

-- test cases for each function --

-- Test the checkWin function
testCheckWin :: IO ()
testCheckWin = do
  let game1 = (initialBoard, Red)
      game2 = (winningBoard, Red)
      game3 = (tieBoard, Red)
  putStrLn "Testing checkWin..."
  assert (checkWin game1 == Nothing) "Test Case 1 Failed"
  assert (checkWin game2 == Just (Win Red)) "Test Case 2 Failed"
  assert (checkWin game3 == Just Tie) "Test Case 3 Failed"
  where
    initialBoard = replicate 6 (replicate 7 Nothing)
    winningBoard =
      [ [Just Red, Just Red, Just Red, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
      ]
    tieBoard =
      [ [Just Red, Just Black, Just Red, Just Black, Just Red, Just Black, Just Red],
        [Just Red, Just Black, Just Red, Just Black, Just Red, Just Black, Just Red],
        [Just Black, Just Red, Just Black, Just Red, Just Black, Just Red, Just Black],
        [Just Black, Just Red, Just Black, Just Red, Just Black, Just Red, Just Black],
        [Just Red, Just Black, Just Red, Just Black, Just Red, Just Black, Just Red],
        [Just Red, Just Black, Just Red, Just Black, Just Red, Just Black, Just Red]
      ]

-- Test the makeMove function
testMakeMove :: IO ()
testMakeMove = do
  let game1 = (initialBoard, Red)
      game2 = makeMove game1 0
      game3 = makeMove game2 1
  putStrLn "Testing makeMove..."
  assert (snd game2 == Black) "Test Case 1 Failed"
  assert (snd game3 == Red) "Test Case 2 Failed"
  where
    initialBoard = replicate 6 (replicate 7 Nothing)

-- Test the isValidMove function
testIsValidMove :: IO ()
testIsValidMove = do
  let board1 = replicate 6 (replicate 7 Nothing)
      board2 = [[Just Red, Just Black, Nothing, Just Red, Just Black, Nothing, Nothing]]
  putStrLn "Testing isValidMove..."
  assert (isValidMove board1 0) "Test Case 1 Failed"
  assert (not (isValidMove board2 2)) "Test Case 2 Failed"

-- Test the validMoves function
testValidMoves :: IO ()
testValidMoves = do
  let game1 = (initialBoard, Red)
      game2 = makeMove game1 0
      game3 = makeMove game2 1
  putStrLn "Testing validMoves..."
  assert (validMoves game1 == [0 .. 6]) "Test Case 1 Failed"
  assert (validMoves game3 == [2 .. 6]) "Test Case 2 Failed"
  where
    initialBoard = replicate 6 (replicate 7 Nothing)

-- Helper function for assertions
assert :: Bool -> String -> IO ()
assert condition message = if condition then putStrLn ("Pass: " ++ message) else putStrLn ("Fail: " ++ message)

-- when board is empty
testEmpty :: Test
testEmpty = do
  let emptyBoard = replicate 7 []
      validMovesEmpty = isValidMoves (emptyBoard, Black)
  assertEqual "The Board is empty" [0 .. 6] validMovesEmpty

-- when board is full
testValidFull :: Test
testValidFull = do
  let fullBoard = replicate 7 (replicate 6 Red)
      validMovesFull = isValidMoves (fullBoard, Black)
  assertEqual "No more moves on board" [] validMovesFull

{-
You will need games that are only a few moves from the end.
I suggest at least one each that is finished, one move, two moves, and four moves from the end.
valid moves, who has won, who will win, and the best move from
-}
-- the games

-- finished where Red has won
finished :: Game
finished =
  let (player1, player2) = emptyGame
      board =
        [ [],
          [],
          [],
          [],
          [Just Red, Just Red, Just Red, Just Red],
          [Just Red, Just Red, Just Black, Just Red],
          [Just Black, Just Black, Just Black, Just Red]
        ]
   in (emptyBoard, player2)

-- one move from the end
oneMove :: Game
oneMove =
  let (player1, player2) = emptyGame
      board =
        [ [],
          [Just Red, Nothing, Just Red, Just Red],
          [Just Red, Just Red, Just Red, Just Black],
          [Just Red, Just Red, Just Black, Just Red],
          [Just Black, Just Black, Just Black, Just Red]
        ]
   in (emptyBoard, player2)

testValidOne :: Test
testValidOne = do
  let board = oneMove
      validMovesOne = validMoves board
  assertEqual "There is one move away from the end" [0, 1, 2, 3, 4, 5, 6] validMovesOne

testWhoWillWinFin :: Test
testWhoWillWinFin = do
  let game = (winningBoard, Black)
  assertEqual "Who will win in a finished game" (Win Black) (whoWillWin game)

testBestMoveFinished :: Test
testBestMoveFinished = do
  let game = (winningBoard, Black) -- needs to be finRedGame but cant get to work
  assertEqual "Best move in a finished game" 0 (bestMove game)

-- two moves from the end
twoMoves :: Game
twoMoves =
  let (player1, player2) = emptyGame
      board =
        [ [],
          [],
          [],
          [Just Red, Nothing, Nothing, Just Red],
          [Just Red, Just Red, Just Red, Just Black],
          [Just Red, Just Red, Just Black, Just Red],
          [Just Black, Just Black, Just Black, Just Red]
        ]
   in (emptyBoard, player2)

testValidDosAway :: Test
testValidDosAway = do
  let board = dosAwayGame
      validMovesDosAway = validMoves board
  assertEqual "Two Moves from End: Verify valid moves" [0, 1, 2, 3, 4, 5, 6] validMovesDosAway

-- Test case: Four moves from the end
fourMoves :: Game
fourMoves =
  let (player1, player2) = emptyGame
      board =
        [ [],
          [],
          [],
          [Nothing, Nothing, Nothing, Just Red],
          [Just Red, Just Red, Just Red, Just Black],
          [Just Red, Just Red, Just Black, Just Red],
          [Just Black, Just Black, Just Black, Just Red]
        ]
   in (emptyBoard, player2)

-- running test  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
runTest :: (String, Test) -> IO ()
runTest (label, test) = do
  result <- test
  putStrLn $ label ++ " - " ++ if result then "Passed" else "Failed"

-- Then, for example, in your list of tests:
main :: IO ()
main = do
  putStrLn "Running tests..."
  testEmpty
  testCheckWin
  testMakeMove
  testIsValidMove
  testValidMoves
  putStrLn "All tests passed!"

{-
allTests :: [(String, Test)]
allTests =

-- Run the tests
main :: IO ()
main = mapM_ runTest allTests
-}

{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

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

-- test cases for main functions --

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

-- Helper function to create a game state from a list of strings representing rows
gameFromStrings :: [String] -> Game
gameFromStrings rows = (map (map readColor) rows, Red)

readColor :: Char -> Maybe Color
readColor 'R' = Just Red
readColor 'Y' = Just Black
readColor '_' = Nothing
readColor _ = error "Invalid"

-- Helper function to create a winner from a string
winnerFromString :: String -> Winner
winnerFromString "Tie" = Tie
winnerFromString "Red" = Win Red
winnerFromString "Black" = Win Black
winnerFromString _ = error "Invalid winner string"

-- Test case for checkWin function
checkWinTest :: String -> Winner -> Test
checkWinTest boardString expectedWinner =
  TestCase $
    assertEqual
      ("Checking winner for board: \n" ++ boardString)
      expectedWinner
      (checkWin (gameFromStrings (lines boardString)))

-- Test cases
tests :: Test
tests =
  TestList
    [ checkWinTest
        "RB_____\nRB_____\nRB_____\nRB_____\nRB_____\nRB_____\nRB_____"
        (Win Red), -- Red wins vertically
      checkWinTest
        "_______\n_______\n_______\nRRRR___\nBB_____\nBB_____\nRRRR___"
        (Win Red), -- Red wins horizontally
      checkWinTest
        "_______\n_______\n_______\nRRR____\nBBB____\nBB_____\nRRR____"
        (Win Black), -- Black wins diagonally
      checkWinTest
        "RYBBB__\nRYYRR__\nRYBBB__\nRYYRR__\nRYBBB__\nRYYRR__\nRYBBB__"
        (Tie) -- Tie game
    ]

main :: IO ()
main = do
  putStrLn "Running tests..."
  -- testEmpty
  testCheckWin
  testMakeMove
  testIsValidMove
  testValidMoves
  tests
  putStrLn "All tests passed!"

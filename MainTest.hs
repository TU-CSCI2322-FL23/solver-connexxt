module TestCases where

import Connect
import Control.Monad
import Data.List
import System.IO

type Test = IO Bool

type TestResult = (String, Bool)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO Bool
assertEqual label expected actual = do
  let result = expected == actual
  putStrLn $ label ++ " - " ++ if result then "Passed" else "Failed"
  pure result

emptyGame :: Game
emptyGame = (emptyBoard, Red)

player1 :: Color
player1 = Black

player2 :: Color
player2 = Red

-- test cases for each function --

main :: IO ()
main = do
  putStrLn "Running tests..."
  testCheckWin
  testMakeMove
  testIsValidMove
  testValidMoves
  putStrLn "All tests passed!"

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
      [ [Just Red, Just Red, Just Red, Nothing, Nothing, Nothing, Nothing]
      , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
      , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
      , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
      , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
      , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
      ]
    tieBoard =
      [ [Just Red, Just Black, Just Red, Just Black, Just Red, Just Black, Just Red]
      , [Just Red, Just Black, Just Red, Just Black, Just Red, Just Black, Just Red]
      , [Just Black, Just Red, Just Black, Just Red, Just Black, Just Red, Just Black]
      , [Just Black, Just Red, Just Black, Just Red, Just Black, Just Red, Just Black]
      , [Just Red, Just Black, Just Red, Just Black, Just Red, Just Black, Just Red]
      , [Just Red, Just Black, Just Red, Just Black, Just Red, Just Black, Just Red]
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
  assert (validMoves game1 == [0..6]) "Test Case 1 Failed"
  assert (validMoves game3 == [2..6]) "Test Case 2 Failed"
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


{-You will need games that are only a few moves from the end.
I suggest at least one each that is finished, one move, two moves, and four moves from the end. -}

--the games

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
  let (_, player) = emptyGame
      board =
        [ [],
          [Just Red, Nothing, Just Red, Just Red],
          [Just Red, Just Red, Just Red, Just Black],
          [Just Red, Just Red, Just Black, Just Red],
          [Just Black, Just Black, Just Black, Just Red]
        ]
   in (emptyBoard, player)

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




-- Test case: One move from the end
testValidUnoAway :: Test
testValidUnoAway = do
  let board = unoAwayGame
      validMovesUnoAway = validMoves board
  assertEqual "One Move from End: Verify valid moves" [0, 1, 2, 3, 4, 5, 6] validMovesUnoAway

-- Test case: two move from the end
testValidDosAway :: Test
testValidDosAway = do
  let board = dosAwayGame
      validMovesDosAway = validMoves board
  assertEqual "Two Moves from End: Verify valid moves" [0, 1, 2, 3, 4, 5, 6] validMovesDosAway

-- Test case: Four moves from the end
testValidCuatroAway :: Test
testValidCuatroAway = do
  let board = cuatroAwayGame
      validMovesCuatroAway = validMoves board
  assertEqual "Four Moves from End: Verify valid moves" [0, 1, 2, 3, 4, 5, 6] validMovesCuatroAway


-- tests for who will win - - - - - - - - - - - - - - - - - - - - - - - -

-- Test case: Finished game
testWhoWillWinFin :: Test
testWhoWillWinFin = do
  let game = (winningBoard, Black)
  assertEqual "Who will win in a finished game" (Win Black) (whoWillWin game)

-- Test case: One move from the end
testWhoWillWinOneFromEnd :: Test
testWhoWillWinOneFromEnd = do
  let game = unoAwayGame
  assertEqual "Who will win one move from the end" (Win Red) (whoWillWin game)

-- Test case: Two moves from the end
testWhoWillWinTwoFromEnd :: Test
testWhoWillWinTwoFromEnd = do
  let game = dosAwayGame
  assertEqual "Who will win two moves from the end" Stalemate (whoWillWin game)

-- Test case: Four moves from the end
testWhoWillWinFourFromEnd :: Test
testWhoWillWinFourFromEnd = do
  let game = cuatroAwayGame
  assertEqual "Who will win four moves from the end" (Win Black) (whoWillWin game)

-- tests for best move - - - - - - - - - - - - - - - - - - - - - - - - -

-- Test case: Finished game
testBestMoveFinished :: Test
testBestMoveFinished = do
  let game = (winningBoard, Black) -- needs to be finRedGame but cant get to work
  assertEqual "Best move in a finished game" 0 (bestMove game)

-- Test case: One move from the end
testBestMoveOneFromEnd :: Test
testBestMoveOneFromEnd = do
  let game = unoAwayGame
  assertEqual "Best move one move from the end" 6 (bestMove game)

-- Test case: Two moves from the end
testBestMoveTwoFromEnd :: Test
testBestMoveTwoFromEnd = do
  let game = dosAwayGame
  assertEqual "Best move two moves from the end" 3 (bestMove game)

-- Test case: Four moves from the end
testBestMoveFourFromEnd :: Test
testBestMoveFourFromEnd = do
  let game = cuatroAwayGame
  assertEqual "Best move four moves from the end" 2 (bestMove game)





-- running test  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
runTest :: (String, Test) -> IO ()
runTest (label, test) = do
  result <- test
  putStrLn $ label ++ " - " ++ if result then "Passed" else "Failed"

-- Then, for example, in your list of tests:
allTests :: [(String, Test)]
allTests =
  [ ("\nEmpty Board Test -- Test Valid Moves", testValidEmpty),
    ("\nFull Board Test", testValidFull),
    ("\nOne Move from End Test", testValidUnoAway),
    ("Two Moves from End Test", testValidDosAway),
    ("Four Moves from End Test", testValidCuatroAway),
    ("\nFull Board Test -- Test Winner", testWinnerfin),
    ("One Move from End Test", testWinnerOneFromEnd),
    ("Two Moves from End Test", testWinnerTwoFromEnd),
    ("Four Moves from End Test", testWinnerFourFromEnd),
    ("\nFull Board Test -- WhoWillWin", testWhoWillWinFin),
    ("One Move from End Test", testWhoWillWinOneFromEnd),
    ("Two Moves from End Test", testWhoWillWinTwoFromEnd),
    ("Four Moves from End Test", testWhoWillWinFourFromEnd),
    ("\nFull Board Test -- Best move", testBestMoveFinished),
    ("One Move from End Test", testBestMoveOneFromEnd),
    ("Two Moves from End Test", testBestMoveTwoFromEnd),
    ("Four Moves from End Test", testBestMoveFourFromEnd)
    -- ... (other tests)
    --add checkwin
  ]

-- Run the tests
--main :: IO ()
--main = mapM_ runTest allTests


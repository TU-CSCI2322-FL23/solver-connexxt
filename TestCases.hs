module TestCases where

import Connect
import Control.Monad.Extra
import Control.Monad.Trans.RWS (RWST (runRWST))
import Data.List
import Data.Semigroup
import Test.Grader.Core
import Test.Grader.Eval
import Test.Grader.Parsing
import Test.Grader.Parsing.Util
import Test.Grader.Rubric
import Test.Grader.Tests

-- player1 = (Fred, Red)

-- player2 = (James, Black)

-- main :: IO ()

makeMove :: Grader String
makeMove = assess "checkWin" 3 $ do
  it "returns Just Red when there is a vertical win for Red" $
    checkWin
      check
      [ [Just Red, Just Red, Just Red, Just Red],
        [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing]
      ]
      `shouldBe` Just Red

  it "returns Just Black when there is a horizontal win for Black" $
    checkWin
      [ [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing],
        [Just Black, Just Black, Just Black, Just Black],
        [Nothing, Nothing, Nothing, Nothing]
      ]
      `shouldBe` Just Black

  it "returns Just Red when there is a diagonal win for Red" $
    checkWin
      [ [Just Red, Nothing, Nothing, Nothing],
        [Nothing, Just Red, Nothing, Nothing],
        [Nothing, Nothing, Just Red, Nothing],
        [Nothing, Nothing, Nothing, Just Red]
      ]
      `shouldBe` Just Red

  it "returns Nothing when there is no winner" $
    checkWin
      [ [Just Red, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Just Black]
      ]
      `shouldBe` Nothing

main :: Grader String
main = describe "final" $ do
  describe "sprint" $ do
    makeMove

runTests :: Int -> Bool -> IO ()
runTests verb force = do
  let a = runGrader main
  format <- makeFormat verb force "project"
  runRWST a () format
  return ()

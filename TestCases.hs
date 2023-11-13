module TestCases where

import Connect
import Control.Monad.Extra
import Control.Monad.Trans.RWS
import Data.List
import Data.Semigroup
import Test.Grader.Core
import Test.Grader.Eval
import Test.Grader.Parsing
import Test.Grader.Parsing.Util
import Test.Grader.Rubric
import Test.Grader.Tests

player1 = (Fred, Red)

player2 = (James, Black)

main :: IO ()
main = hspec $ do
  describe "checkWin" $ do
    it "returns Just Red when there is a vertical win for Red" $
      checkWin
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

{-
testIsValidMove :: Grader String
testIsValidMove = assess "isValidMove" 3 $ do
    let tf = isValidMove tinyDict
    check "that \"MAKE\" and \"I\" are valid moves" $ "MAKE" `shouldSatisfy` (tf "MKEKIOCHAOX") <> "I" `shouldSatisfy` (tf "MKEKIOCHAOX")
    check "that moves that can't be made are invalid" $ "MAKE" `shouldSatisfy`(not . tf "MKKIOCHAOX" )
    check "that moves not in the dictionary are invalid" $ "MAKI" `shouldSatisfy`(not . tf "MKKIOCHAOX")

testValidMoves :: Grader Int
testValidMoves = assess "validMoves" 4 $ do
    let insertColor1 = 4
        insertColor2 = 4
        insertColor3 = 4
        moves1 = 4
        moves2 = 4
        moves3 = 4 -- moves is it showing how it is a connect 4 and shows how it is not a connect 4
    check "that validMoves are valid lol" $
    --check that it makes a connect 4 and when it does not make a connect 4

       -- hand2 = "EIFYOZWFKC"
        --hand3 = "AAJNURWLGG"
        --moves1 = ["DIE","DO","I","ME","MOVE","MOVIE","PM"]
       -- moves2 = ["FEW","I","IF","KEY","OF","OFF","OFFICE","OK","WE","WIFE"]
        --moves3 = ["A","GUN","LAW","RUN","WAR"]
    --check "that validMoves the valid moves for \"PEMDOVZIJM\" with shortDict" $ (sort $ validMoves shortDict hand1) `shouldBe` moves1
    --check "that validMoves finds the valid moves for \"EIFYOZWFKC\" with shortDict" $ (sort $ validMoves shortDict hand2) `shouldBe` moves2
    --check "that validMoves finds the valid moves for \"AAJNURWLGG\" with shortDict" $ (sort $ validMoves shortDict hand3) `shouldBe` moves3
    --check "that validMoves finds the valid moves for the empty list" $ (sort $ validMoves shortDict "") `shouldBe` []

tree :: Grader String
tree = describe "connexxt" $ do
    describe "Nov 8" $ do
      testIsValidMove
      --benchmarks

runTests :: Int -> Bool -> IO ()
runTests verb force = do
    let a = runGrader tree
    format <- makeFormat verb force "projectDesc.yaml"
    runRWST a () format
    return ()
-}
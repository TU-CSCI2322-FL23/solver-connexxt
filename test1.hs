module TestCases where
import Connect
--module Testing where
--import Scavenge
import Data.Semigroup
--import Dictionaries
import Control.Monad.Extra
import Test.Grader.Core
import Test.Grader.Rubric
import Test.Grader.Eval
import Test.Grader.Tests
import Test.Grader.Parsing.Util
import Test.Grader.Parsing
import Data.List
import Control.Monad.Trans.RWS

player1 = (Fred, Red)
player2=(James, Black)

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

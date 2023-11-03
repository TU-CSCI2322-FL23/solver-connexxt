module TestCases where
import connect

player1 = (Fred, Red)
player2=(James, Black)

testValidMoves :: Grader String
testValidMoves = assess "validMoves" 4 $ do
    let insertColor1 = --int





       -- hand2 = "EIFYOZWFKC"
        --hand3 = "AAJNURWLGG"
        --moves1 = ["DIE","DO","I","ME","MOVE","MOVIE","PM"]
       -- moves2 = ["FEW","I","IF","KEY","OF","OFF","OFFICE","OK","WE","WIFE"]
        --moves3 = ["A","GUN","LAW","RUN","WAR"]
    check "that validMoves the valid moves for \"PEMDOVZIJM\" with shortDict" $ (sort $ validMoves shortDict hand1) `shouldBe` moves1
    check "that validMoves finds the valid moves for \"EIFYOZWFKC\" with shortDict" $ (sort $ validMoves shortDict hand2) `shouldBe` moves2
    check "that validMoves finds the valid moves for \"AAJNURWLGG\" with shortDict" $ (sort $ validMoves shortDict hand3) `shouldBe` moves3
    check "that validMoves finds the valid moves for the empty list" $ (sort $ validMoves shortDict "") `shouldBe` []

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

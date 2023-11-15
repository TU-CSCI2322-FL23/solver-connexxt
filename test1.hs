module TestCases where

import Connect
import hspec

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
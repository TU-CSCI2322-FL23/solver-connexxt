{-# LANGUAGE BlockArguments #-}
module TestCases where

import Connect
import Control.Monad.Extra
import Control.Monad.Trans.RWS (RWST (runRWST))
import Data.List
import Data.Semigroup
import System.IO
import Test.Grader.Core
import Test.Grader.Eval
import Test.Grader.Parsing
import Test.Grader.Parsing.Util
import Test.Grader.Rubric
import Test.Grader.Tests

-- player1 = (Fred, Red)

-- player2 = (James, Black)

-- file input to read in and show who would win in each game

      -- winner of first game should be Just Red for vertical win
      [
       [Just Red, Just Red, Just Red, Just Red],
        [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing]
      ]

      -- winner of second game should be Just Black for horizontal win
[
       [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing],
        [Just Black, Just Black, Just Black, Just Black],
        [Nothing, Nothing, Nothing, Nothing]
]
      -- winner of third game should be Just Red for diagonal win
[
       [Just Red, Nothing, Nothing, Nothing],
        [Nothing, Just Red, Nothing, Nothing],
        [Nothing, Nothing, Just Red, Nothing],
        [Nothing, Nothing, Nothing, Just Red]
]
      -- winner of fourth game should be Nothing for no winner
[
      [Just Red, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Just Black]
      
]

-- estimate 2

--games close to the end

[
       [Just Red, Just Red, Just Red, Nothing],
        [Just Black, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Just Black, Nothing],
        [Nothing, Just Black, Nothing, Just Black]
]

--games where one player is dominating 

[
       [Nothing, Nothing, Nothing, Nothing],
        [Just Black, Just Red, Nothing, Just Red],
        [Just Black, Nothing, Just Red, Just Red],
        [Just Black, Nothing, Nothing, Just Red]
]

--games that are evenly matched 

[
      [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing],
        [Just Red, Just Red, Just Red, Just Red],
        [Just Red, Just Red, Just Black, Just Black]
      
]

[
      [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing]
      
]

--games near the start

[
      [Just Red, Nothing, Nothing, Nothing],+

























































      +++++++++++++++++++++++++
      +++++++++                      




























       






         




































































































         +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



         
        [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Just Black]
      
]
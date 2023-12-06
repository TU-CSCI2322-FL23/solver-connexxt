module MainIO where

import Connect
import Control.Monad
import System.Console.ANSI ()
import System.IO

main = do
  let list = []
  handle <- openFile "TextFile.txt" ReadMode
  contents <- hGetContents handle
  let singlewords = words contents
      list = f singlewords
  print list
  hClose handle

f :: [String] -> [String]
f = map read

showGame :: Game -> String
showGame (board, turn) = unlines (showColor turn : showBoard board)

showColor Red = "R"
showColor Black = "Y"

showMColor (Just c) = showColor c
showMColor (Nothing) = "_"

showBoard board = [concat $ map showMColor col | col <- board]

readGame :: String -> Game
readGame str =
  let ls = lines str
      turn = readColor (head ls)
      board = map (map readMColor) (tail ls)
   in (board, turn)

readColor :: String -> Color
readColor "R" = Red
readColor "Y" = Black
readColor _ = error "Invalid"

readMColor :: Char -> Maybe Color
readMColor 'R' = Just Red
readMColor 'Y' = Just Black
readMColor '_' = Nothing
readMColor _ = error "Invalid"

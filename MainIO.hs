import Control.Monad
import System.IO

main = do
  let list = []
  handle <- openFile "TextFile.hs" ReadMode
  contents <- hGetContents handle
  let singlewords = words contents
      list = f singlewords
  print list
  hClose handle

f :: [String] -> [Int]
f = map read
module Main where
import Connect
import Control.Monad
import System.IO
import System.Environment
import System.Exit

oldmain = do
  let list = []
  handle <- openFile "TextFile.hs" ReadMode
  contents <- hGetContents handle
  let singlewords = words contents
      list = f singlewords
  print list
  hClose handle

f :: [String] -> [Int]
f = map read

defaultDepth :: Int 
defaultDepth = 5
main :: IO()
main = do 
    args <- getArgs 
    case args of
        ["-h"] -> printHelp >> exitSuccess 
        ["--help"] -> printHelp >> exitSuccess
        ["-w", filename] -> processFile filename True defaultDepth 
        ["--winner", filename] -> processFile filename True defaultDepth 
        ["-d", depth, filename] -> processFile filename False (read depth :: Int)
        ["--depth", depth, filename] -> processFile filename False (read depth :: Int)
        [filename] -> processFile filename False defaultDepth 
        _ -> printUsage >> exitFailure 

printHelp :: IO () 
printHelp = putStrLn $
    "Your Program\n\n" ++
    "Options:\n" ++
    "   -w, --winner            Output the best move with no cutoff depth.\n"++
    "   -d <num>, --depth <num>       Specify the cutoff depth (default is 5).\n"++
    "   -h, --help           Display this help message."

printUsage :: IO ()
printUsage = putStrLn "Usage: your_program [-w] [-d <num>] <filename>"

processFile :: FilePath -> Bool -> Int -> IO ()
processFile filename exhaustive depth = do
    contents <- readFile filename 
    let initalGame = readGame contents 
        cutoff = if exhaustive then maxBound else depth 
        b = findBestMove initalGame cutoff 
        updatedGame = makeMove initalGame b 
    putStrLn $ "Inital game state:\n" ++ showGame initalGame 
    putStrLn $ "Updated Game state:\n" ++ showGame updatedGame 
    -- read the board from file
    -- perform stuff based on flags 
    -- number 8 from second sprint 

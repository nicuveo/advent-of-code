module Main where

import "this" Prelude

import Data.Char
import Data.List             (dropWhileEnd)
import Safe
import System.Environment
import System.Exit
import System.FilePath.Posix
import Text.Printf

import Day01 qualified
import Day02 qualified
import Day03 qualified
import Day04 qualified
import Day05 qualified
import Day06 qualified
import Day07 qualified
import Day08 qualified
import Day09 qualified
import Day10 qualified
import Day11 qualified
import Day12 qualified

printUsageAndDie :: IO a
printUsageAndDie = do
  cmd <- getProgName
  die $ "usage: " ++ cmd ++ " path_to_input_dir day"

getInput :: Int -> String -> IO String
getInput day dir = dropWhileEnd isSpace <$> readFile filename
  where filename = dir </> printf "%02d" day ++ ".in"

mains :: [String -> IO ()]
mains =
  [ Day01.run, Day02.run, Day03.run, Day04.run, Day05.run, Day06.run
  , Day07.run, Day08.run, Day09.run, Day10.run, Day11.run, Day12.run
  ]

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) printUsageAndDie
  let folder = head args
  for_ (tail args) \n -> do
    day <- maybe printUsageAndDie pure $ readMay n
    putStrLn $ printf "### Day %02d ###" day
    input <- getInput day folder
    (mains !! pred day) input

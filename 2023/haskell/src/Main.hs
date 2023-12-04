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
import Day13 qualified
import Day14 qualified
import Day15 qualified
import Day16 qualified
import Day17 qualified
import Day18 qualified
import Day19 qualified
import Day20 qualified
import Day21 qualified
import Day22 qualified
import Day23 qualified
import Day24 qualified
import Day25 qualified

printUsageAndDie :: IO a
printUsageAndDie = do
  cmd <- getProgName
  die $ "usage: " ++ cmd ++ " path_to_input_dir day"

getInput :: Int -> String -> IO String
getInput day dir = dropWhileEnd isSpace <$> readFile filename
  where filename = dir </> printf "%02d" day ++ ".in"

mains :: [String -> IO ()]
mains =
  [ Day01.main, Day02.main, Day03.main, Day04.main, Day05.main
  , Day06.main, Day07.main, Day08.main, Day09.main, Day10.main
  , Day11.main, Day12.main, Day13.main, Day14.main, Day15.main
  , Day16.main, Day17.main, Day18.main, Day19.main, Day20.main
  , Day21.main, Day22.main, Day23.main, Day24.main, Day25.main
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

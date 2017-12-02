-- module

module Main where



-- imports

import           Control.Monad
import           Data.Char
import           Data.List
import           Safe
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           Text.Printf

import           Common
import           Day01
import           Day02
import           Day03
import           Day04
import           Day05
import           Day06
import           Day07
import           Day08
import           Day09
import           Day10
import           Day11
import           Day12
import           Day13
import           Day14
import           Day15
import           Day16
import           Day17
import           Day18
import           Day19
import           Day20
import           Day21
import           Day22
import           Day23
import           Day24
import           Day25



-- helpers

printUsageAndDie :: String -> IO ()
printUsageAndDie cmd = die $ "usage: " ++ cmd ++ " path_to_input_dir [days...]"

getInput :: String -> Int -> IO String
getInput dir day = dropWhileEnd isSpace <$> readFile filename
  where filename = dir </> printf "%02d" day ++ ".in"

solutions :: [(Int, [Solution])]
solutions = zip [1..] [[day01_1, day01_2],
                       [day02_1, day02_2],
                       [day03_1, day03_2],
                       [day04_1, day04_2],
                       [day05_1, day05_2],
                       [day06_1, day06_2],
                       [day07_1, day07_2],
                       [day08_1, day08_2],
                       [day09_1, day09_2],
                       [day10_1, day10_2],
                       [day11_1, day11_2],
                       [day12_1, day12_2],
                       [day13_1, day13_2],
                       [day14_1, day14_2],
                       [day15_1, day15_2],
                       [day16_1, day16_2],
                       [day17_1, day17_2],
                       [day18_1, day18_2],
                       [day19_1, day19_2],
                       [day20_1, day20_2],
                       [day21_1, day21_2],
                       [day22_1, day22_2],
                       [day23_1, day23_2],
                       [day24_1, day24_2],
                       [day25_1, day25_2]]



-- main

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ getProgName >>= printUsageAndDie
  let daysToRun = read <$> drop 1 args
      run day fun = fun <$> getInput (head args) day
      printDay (day, parts) = do
        printf "# day %d\n" day
        forM_ parts $ putStrLn <=< run day
  mapM_ printDay $ if null daysToRun
                   then solutions
                   else map (at solutions . pred) daysToRun

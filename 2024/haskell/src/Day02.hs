module Day02 where


-- import

import AOC
import "this" Prelude

import Text.Parsec


-- input

type Input = [Report]
type Report = [Int]

parseInput :: String -> Input
parseInput = parseLinesWith $ many1 number


-- solution

isSafe :: Report -> Bool
isSafe report = (allIncreasing || allDecreasing) && allCloseEnough
  where
    allIncreasing   = and $ zipWith (<)         report (tail report)
    allDecreasing   = and $ zipWith (>)         report (tail report)
    allCloseEnough  = and $ zipWith closeEnough report (tail report)
    closeEnough x y = abs (x-y) <= 3

isPotentiallySafe :: Report -> Bool
isPotentiallySafe = any isSafe . generateSubLists []
  where
    generateSubLists _      []     = []
    generateSubLists before (x:xs) = (before ++ xs) : generateSubLists (before ++ [x]) xs

part1 :: Input -> Int
part1 = countIf isSafe

part2 :: Input -> Int
part2 = countIf isPotentiallySafe


-- main

example :: String
example = "\
\7 6 4 2 1\n\
\1 2 7 8 9\n\
\9 7 6 2 1\n\
\1 3 2 4 5\n\
\8 6 4 4 1\n\
\1 3 6 7 9"

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

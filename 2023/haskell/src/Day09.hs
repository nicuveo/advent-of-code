module Day09 where


-- import

import AOC
import "this" Prelude

import Text.Parsec


-- input

type Input = [[Int]]

parseInput :: String -> Input
parseInput = parseLinesWith $ many1 number


-- solution

predictNext :: [Int] -> Int
predictNext xs
  | all (== 0) xs = 0
  | otherwise     = last xs + predictNext (zipWith (-) (tail xs) xs)

part1 :: Input -> Int
part1 = sum . map predictNext

part2 :: Input -> Int
part2 = sum . map (predictNext . reverse)


-- main

example :: String
example = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"

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

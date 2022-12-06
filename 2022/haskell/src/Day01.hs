module Main where


-- import

import Data.List       (sort)
import Data.List.Split

import AOC


-- input

type Input = [Int]

parseInput :: String -> Input
parseInput = map (sum . map read) . splitOn [""] . lines


-- solution

part1 :: Input -> Int
part1 = maximum

part2 :: Input -> Int
part2 = sum . take 3 . reverse . sort


-- main

main :: IO ()
main = aocMain 01 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"

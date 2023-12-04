module Day25 where


-- import

import AOC
import "this" Prelude

import Text.Parsec
import Text.Parsec.Char


-- input

type Input = String

parseInput :: String -> Input
parseInput = parseLinesWith line
  where line = undefined


-- solution

part1 :: Input -> Int
part1 = undefined

part2 :: Input -> Int
part2 = undefined


-- main

example :: String
example = ""

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

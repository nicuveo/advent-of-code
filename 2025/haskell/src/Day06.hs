module Day06 where

import "this" Prelude

import AOC

import Text.Parsec
import Text.Parsec.Char


type Input = String

parseInput :: String -> Input
parseInput = parseLinesWith line
  where line = undefined


part1 :: Input -> Int
part1 = undefined

part2 :: Input -> Int
part2 = undefined


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

module Main where


-- import

import Data.Char
import Data.List       qualified as L
import Data.List.Split

import AOC


-- input

type Input = [String]

parseInput :: String -> Input
parseInput = lines


-- solution

priority :: Char -> Int
priority c
  | isLower c = ord c - ord 'a' + 1
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = error $ c : " is not a valid item"

part1 :: Input -> Int
part1 = sum . map (priority . head . uncurry L.intersect . splitInTwo)
  where
    splitInTwo l = L.splitAt (length l `div` 2) l

part2 :: Input -> Int
part2 = sum . map (priority . head . intersect3) . chunksOf 3
  where
    intersect3 [a,b,c] = L.intersect a $ L.intersect b c
    intersect3 _       = error $ "group with less than three lines"


-- main

main :: IO ()
main = aocMain 03 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"

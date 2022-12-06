module Main where


-- import

import Data.Foldable
import Text.Parsec
import Text.Parsec.Char

import AOC


-- input

type Range = (Int, Int)
type Input = [(Range,Range)]

parseInput :: String -> Input
parseInput = parseLinesWith line
  where
    line = do
      x <- range
      char ','
      y <- range
      pure (x, y)
    range = do
      x <- number
      char '-'
      y <- number
      pure (x, y)


-- solution

within :: Range -> Range -> Bool
within (x1, y1) (x2, y2) = x1 >= x2 && y1 <= y2

part1 :: Input -> Int
part1 = countIf oneWithinTheOther
  where
    oneWithinTheOther (range1, range2) =
      (range1 `within` range2) || (range2 `within` range1)

part2 :: Input -> Int
part2 = countIf overlaps
  where
    overlaps :: (Range, Range) -> Bool
    overlaps (r1@(x1, y1), r2@(x2, _y2))
      | x1 > x2 = overlaps (r2, r1)
      | otherwise = y1 >= x2


-- main

main :: IO ()
main = aocMain 04 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

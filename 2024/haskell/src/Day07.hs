module Day07 where


-- import

import AOC
import "this" Prelude

import Data.List.NonEmpty qualified as NE
import Text.Parsec


-- input

type Input = [(Int, NonEmpty Int)]

parseInput :: String -> Input
parseInput = parseLinesWith line
  where
    line = do
      total <- number
      symbol ":"
      numbers <- many1 number
      pure (total, NE.fromList numbers)


-- solution

part1 :: Input -> Int
part1 calibrations = sum do
  (total, x NE.:| xs) <- calibrations
  guard $ any (total ==) $ snd $ foldl' step (total, [x]) xs
  pure total
  where
    step (total, accum) x = (total, filter (<= total) $ [(+x), (*x)] <*> accum)

part2 :: Input -> Int
part2 calibrations = sum do
  (total, x NE.:| xs) <- calibrations
  guard $ any (total ==) $ snd $ foldl' step (total, [x]) xs
  pure total
  where
    step (total, accum) x = (total, filter (<= total) $ apply x accum)
    apply y accum = do
      x <- accum
      [x+y, x*y, read (show x ++ show y)]


-- main

example :: String
example = "\
\190: 10 19\n\
\3267: 81 40 27\n\
\83: 17 5\n\
\156: 15 6\n\
\7290: 6 8 6 15\n\
\161011: 16 10 13\n\
\192: 17 8 14\n\
\21037: 9 7 18 13\n\
\292: 11 6 16 20"

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

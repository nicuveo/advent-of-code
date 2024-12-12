module Day01 where


-- import

import AOC
import "this" Prelude


-- input

type Input = ([Int], [Int])

parseInput :: String -> Input
parseInput = unzip . parseLinesWith line
  where
    line = do
      n1 <- number
      n2 <- number
      pure (n1, n2)


-- solution

part1 :: Input -> Int
part1 (l1, l2) = sum $ map abs $ zipWith (-) (sort l1) (sort l2)

part2 :: Input -> Int
part2 (l1, l2) = sum do
  x <- l1
  pure $ x * count x l2


-- main

example :: String
example = "\
\3   4\n\
\4   3\n\
\2   5\n\
\1   3\n\
\3   9\n\
\3   3"

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

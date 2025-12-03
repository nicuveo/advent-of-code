module Day03 where

import "this" Prelude

import Control.Parallel.Strategies
import Data.Char


type Input = [[Int]]

parseInput :: String -> Input
parseInput = map (map digitToInt) . lines


part1 :: Input -> Int
part1 = sum . parMap rpar (findGreatestJoltage 2)

part2 :: Input -> Int
part2 = sum . parMap rpar (findGreatestJoltage 12)

findGreatestJoltage :: Int -> [Int] -> Int
findGreatestJoltage size = go $ replicate size 0
  where
    go candidates = \case
      []     -> foldl1 (\x y -> x*10+y) candidates
      (x:xs) -> select [] candidates x xs

    select mostSignificant leastSignificant x xs =
      case leastSignificant of
        []     -> go mostSignificant xs
        (d:ds) ->
          if x > d && length xs >= length ds
          then go (mostSignificant ++ x : (0 <$ ds)) xs
          else select (mostSignificant ++ [d]) ds x xs


example :: String
example = "\
\987654321111111\n\
\811111111111119\n\
\234234234234278\n\
\818181911112111\n"

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

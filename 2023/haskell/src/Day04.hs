module Day04 where


-- import

import AOC
import "this" Prelude

import Data.HashSet   qualified as S
import Data.IntMap    qualified as M
import Text.Parsec


-- input

type Input = [Int]

parseInput :: String -> Input
parseInput = parseLinesWith do
  symbol "Card"
  number
  symbol ":"
  winningNumbers <- many1 number
  symbol "|"
  myNumbers <- many1 number
  let winningSet = S.fromList winningNumbers
  pure $ countIf (`S.member` winningSet) myNumbers


-- solution

part1 :: Input -> Int
part1 = sum . map \case
  0 -> 0
  n -> 2 ^ (n-1)

part2 :: Input -> Int
part2 results = sum $ map countCards indices
  where
    indices   = M.keys winners
    winners   = M.fromList $ zip [1..] results
    cardCount = M.fromList [(i, countCards i) | i <- indices]
    countCards index =
      let w = winners M.! index
      in  1 + sum [cardCount M.! i | i <- [index+1..index+w]]


-- main

example :: String
example = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

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

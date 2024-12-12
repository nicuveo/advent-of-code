module Day05 where


-- import

import AOC
import "this" Prelude

import Data.HashSet   qualified as S
import Data.List      (insertBy, tails)
import Text.Parsec


-- input

type Input = (Constraints, [Update])
type Constraints = HashSet (Int, Int)
type Update = [Int]

parseInput :: String -> Input
parseInput = parseWith do
  constraints <- many1 $ try constraint
  updates     <- many1 $ try update
  pure (S.fromList constraints, updates)
  where
    constraint = do
      n1 <- number
      symbol "|"
      n2 <- number
      pure (n2, n1)
    update = number `sepBy1` symbol ","


-- solution

allPairs :: [a] -> [(a,a)]
allPairs = concatMap go . tails
  where
    go []     = []
    go (x:xs) = map (x,) xs

part1 :: Input -> Int
part1 (constraints, updates) = sum do
  update <- updates
  guard $ not $ any (`S.member` constraints) $ allPairs update
  pure $ update !! (length update `div` 2)

part2 :: Input -> Int
part2 (constraints, updates) = sum do
  update <- updates
  guard $ any (`S.member` constraints) $ allPairs update
  let sortedList = foldl' (flip $ insertBy go) [] update
  pure $ sortedList !! (length sortedList `div` 2)
  where
    go l r = if (r,l) `S.member` constraints then LT else GT


-- main

example :: String
example = "\
\47|53\n\
\97|13\n\
\97|61\n\
\97|47\n\
\75|29\n\
\61|13\n\
\75|53\n\
\29|13\n\
\97|29\n\
\53|29\n\
\61|53\n\
\97|53\n\
\61|29\n\
\47|13\n\
\75|47\n\
\97|75\n\
\47|61\n\
\75|61\n\
\47|29\n\
\75|13\n\
\53|13\n\
\\n\
\75,47,61,53,29\n\
\97,61,53,29,13\n\
\75,29,13\n\
\75,97,47,61,53\n\
\61,13,29\n\
\97,13,75,29,47"


main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  print testInput
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

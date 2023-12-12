module Day12 where


-- import

import AOC
import "this" Prelude

import Data.HashMap.Strict qualified as M
import Text.Parsec


-- input

type Input = [([Spring], [Int])]
data Spring
  = Operational
  | Damaged
  | Unknown
  deriving (Eq)

instance Show Spring where
  show Operational = "."
  show Damaged     = "#"
  show Unknown     = "?"
  showList springs s = concatMap show springs ++ s

parseInput :: String -> Input
parseInput = parseLinesWith do
  springs <- many1 spring
  spaces
  groups  <- number `sepBy` symbol ","
  pure (springs, groups)
  where
    spring = choice
      [ Operational <$ char '.'
      , Damaged     <$ char '#'
      , Unknown     <$ char '?'
      ]


-- solution

type Key = ([Int], Bool)

permutations :: [Spring] -> [Int] -> Int
permutations springs groups = go springs $ M.singleton (groups, True) 1
  where
    go :: [Spring] -> HashMap Key Int -> Int
    go []     states = sum $ map countValid $ M.toList states
    go (c:cs) states = go cs $ M.fromListWith (+) do
      (s, occurences) <- M.toList states
      nextState <- step c s
      pure (nextState, occurences)

    countValid :: (Key, Int) -> Int
    countValid (([],  _), n) = n
    countValid (([0], _), n) = n
    countValid _             = 0

    step :: Spring -> Key -> [Key]
    step Operational (gs    , True ) = [(gs, True)]
    step Operational ([]    , False) = []
    step Operational ((0:gs), False) = [(gs, True)]
    step Operational (_     , False) = []
    step Damaged     ([]    , _    ) = []
    step Damaged     ((0: _), _    ) = []
    step Damaged     ((n:gs), _    ) = [((n-1):gs, False)]
    step Unknown     (gs    , op   ) = concat
      [ step Operational (gs, op)
      , step Damaged     (gs, op)
      ]

part1 :: Input -> Int
part1 = sum . map (uncurry permutations)

part2 :: Input -> Int
part2 = sum . map go
  where
    go (springs, groups) = permutations
      (intercalate [Unknown] $ replicate 5 springs)
      (concat $ replicate 5 groups)


-- main

example :: String
example = "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1"

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

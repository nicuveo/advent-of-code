module Main where


-- import

import Text.Parsec
import Text.Parsec.Char

import AOC


-- input

type Input = [(Char, Char)]

type Match = [Play]
type Play = (Hand, Hand)
data Hand
  = Rock
  | Paper
  | Scissors
  deriving (Bounded, Enum, Eq, Show)

data Strategy
  = Lose
  | Draw
  | Win
  deriving (Eq, Show)

parseInput :: String -> Input
parseInput = map splitPlay . lines
  where
    splitPlay = \case
      [theirs, ' ', mine] -> (theirs, mine)
      _                   -> error "wrong format"


-- solution

handScore :: Hand -> Int
handScore = \case
  Rock     -> 1
  Paper    -> 2
  Scissors -> 3

outcome :: Hand -> Hand -> Int
outcome theirs mine = case (theirs, mine) of
  (Rock, Paper)     -> 6
  (Paper, Scissors) -> 6
  (Scissors, Rock)  -> 6
  _                 -> if theirs == mine then 3 else 0

pickHand :: Hand -> Strategy -> Hand
pickHand hand strat = case strat of
  Lose -> toEnum $ (fromEnum hand - 1) `mod` 3
  Draw -> hand
  Win  -> toEnum $ (fromEnum hand + 1) `mod` 3

part1 :: Input -> Int
part1 = sum . map play
  where
    play (readHand -> theirs, readHand -> mine) =
      outcome theirs mine + handScore mine
    readHand = \case
      'A' -> Rock
      'B' -> Paper
      'C' -> Scissors
      'X' -> Rock
      'Y' -> Paper
      'Z' -> Scissors
      _   -> error "unrecognized hand"

part2 :: Input -> Int
part2 = sum . map play
  where
    play (readHand -> theirs, readStrategy -> strat) =
      let mine = pickHand theirs strat
      in  outcome theirs mine + handScore mine
    readHand = \case
      'A' -> Rock
      'B' -> Paper
      'C' -> Scissors
      _   -> error "unrecognized hand"
    readStrategy = \case
      'X' -> Lose
      'Y' -> Draw
      'Z' -> Win
      _   -> error "unrecognized hand"


-- main

main :: IO ()
main = aocMain 02 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "A Y\nB X\nC Z"

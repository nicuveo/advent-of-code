module Day01 where

import "this" Prelude hiding (Left, Right)

import AOC            hiding (Direction)

import Data.List      qualified as L
import Text.Parsec


type Input = [Instruction]
type Instruction = (Direction, Int)
data Direction = Left | Right
  deriving Show

parseInput :: String -> Input
parseInput = parseLinesWith line
  where
    line = do
      dir <- direction
      value <- number
      pure (dir, value)
    direction = choice
      [ Left  <$ char 'L'
      , Right <$ char 'R'
      ]


apply :: Instruction -> Int -> Int
apply (direction, amount) = case direction of
  Left  -> subtract amount
  Right -> (+amount)


part1 :: Input -> Int
part1 = snd . foldl' step (50, 0)
  where
    step (position, occurences) instruction =
      let newPosition = apply instruction position `mod` 100
      in  (newPosition, occurences + fromEnum (newPosition == 0))

part2 :: Input -> Int
part2 = snd . L.foldl' step (50, 0)
  where
    step (position, occurences) (direction, amount) =
      let
        zeroes = abs $ amount `div` 100
        diff = amount `mod` 100
        newPosition = case direction of
          Left  -> position - diff
          Right -> position + diff
        additionalZero = (newPosition < 1 || newPosition > 99) && position /= 0
      in
        (newPosition `mod` 100, occurences + zeroes + fromEnum additionalZero)

example :: String
example = "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82"

run :: String -> IO ()
run rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

module Day05 where

import "this" Prelude

import AOC
import AOC.RangeSet   as RS

import Data.Maybe
import Text.Parsec


data Input = Input
  { inputRanges      :: RangeSet Int
  , inputIngredients :: [Int]
  }
  deriving Show

parseInput :: String -> Input
parseInput = parseWith do
  ranges <- rangeLine `sepEndBy` newline
  newline
  ingredients <- ingredientLine `sepEndBy` newline
  pure $ Input (RS.fromList ranges) ingredients
  where
    rangeLine = do
      b <- value
      symbol "-"
      e <- value
      pure $ fromJust $ range b e
    ingredientLine =
      value
    value =
      read <$> many1 digit


part1 :: Input -> Int
part1 (Input rangeSet ingredients) = countTrue do
  ingredient <- ingredients
  pure $ ingredient `within` rangeSet

part2 :: Input -> Int
part2 (Input rangeSet _) =
  sum $ map RS.size $ RS.toList rangeSet


example :: String
example = "\
  \3-5\n\
  \10-14\n\
  \16-20\n\
  \12-18\n\
  \\n\
  \1\n\
  \5\n\
  \8\n\
  \11\n\
  \17\n\
  \32\n"

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

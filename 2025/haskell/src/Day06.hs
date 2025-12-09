module Day06 where

import "this" Prelude

import Data.List      (transpose)
import Text.Parsec

import AOC


type Input = [String]

parseInput :: String -> Input
parseInput = lines


part1 :: Input -> Int
part1 = sum . map (compute . reverse) . transpose . map words
  where
    compute = \case
      ("+":numbers) -> sum     $ map read numbers
      ("*":numbers) -> product $ map read numbers
      _ -> error "incorrect input"

data Line
  = Number    Int
  | Operation Symbol
  deriving Show

data Symbol = Addition | Multiplication
  deriving Show

part2 :: Input -> Int
part2 = fst . foldl' go (0, []) . concatMap segment . reverse . transpose
  where
    go (total, buffer) = \case
      Number num    -> (total, num : buffer)
      Operation op  -> (total + compute op buffer, [])
    compute op nums = case op of
      Addition       -> sum     nums
      Multiplication -> product nums
    segment = parseWith do
      spaces
      num <- optionMaybe number
      op  <- optionMaybe operation
      pure $ case (num, op) of
        (Nothing, _)      -> []
        (Just n, Nothing) -> [Number n]
        (Just n, Just o)  -> [Number n, Operation o]
    operation = choice
      [ Addition       <$ symbol "+"
      , Multiplication <$ symbol "*"
      ]


example :: String
example = "\
\123 328  51 64 \n\
\ 45 64  387 23 \n\
\  6 98  215 314\n\
\*   +   *   +  \n"

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

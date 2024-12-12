module Day03 where


-- import

import AOC
import "this" Prelude

import Text.Parsec


-- input

data Instruction
  = Multiplication Int Int
  | EnableMultiplication
  | DisableMultiplication
  deriving (Show, Eq)

type Input = [Instruction]

parseInput :: String -> Input
parseInput = catMaybes . parseWith (many1 maybeInstruction)
  where
    maybeInstruction = tryAll
      [ multiplication
      , enable
      , disable
      , corruptedData
      ]
    multiplication = do
      string "mul("
      n1 <- read <$> many1 digit
      string ","
      n2 <- read <$> many1 digit
      string ")"
      pure $ Just $ Multiplication n1 n2
    enable  = Just EnableMultiplication  <$ string "do()"
    disable = Just DisableMultiplication <$ string "don't()"
    corruptedData = Nothing <$ anyChar


-- solution

part1 :: Input -> Int
part1 = sum . map \case
  Multiplication n1 n2 -> n1 * n2
  _                    -> 0

part2 :: Input -> Int
part2 = sum . flip evalState True . traverse \case
  Multiplication n1 n2 -> do
    isMultiplicationEnabled <- get
    pure $ if isMultiplicationEnabled then n1*n2 else 0
  EnableMultiplication -> do
    put True
    pure 0
  DisableMultiplication -> do
    put False
    pure 0


-- main

example1, example2 :: String
example1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
example2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

main :: String -> IO ()
main rawData = do
  let testInput1 = parseInput example1
      testInput2 = parseInput example2
      realInput  = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput1
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput2
  print $ part2 realInput

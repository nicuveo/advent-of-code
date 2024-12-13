module Day13 where


-- import

import AOC
import "this" Prelude

import Control.Concurrent.Async
import Data.SBV
import Text.Parsec


-- input

type Input = [Machine]

data Machine = Machine
  { aButton :: Point
  , bButton :: Point
  , prize   :: Point
  }
  deriving Show

parseInput :: String -> Input
parseInput = parseWith $ many machine
  where
    machine = do
      a <- button "A"
      b <- button "B"
      p <- prize
      pure $ Machine a b p
    button n = do
      symbol "Button"
      symbol n
      symbol ":"
      symbol "X+"
      x <- number
      symbol ","
      symbol "Y+"
      y <- number
      pure $ Point x y
    prize = do
      symbol "Prize:"
      symbol "X="
      x <- number
      symbol ","
      symbol "Y="
      y <- number
      pure $ Point x y


-- solution

findMinimumCost :: Int -> Machine -> IO (Maybe Integer)
findMinimumCost offset Machine {..} = fmap (getModelValue "cost") $ optLexicographic do
  a <- sInteger "a"
  b <- sInteger "b"
  constrain $
    fromIntegral (px aButton) * a +
    fromIntegral (px bButton) * b .==
    fromIntegral (px prize + offset)
  constrain $
    fromIntegral (py aButton) * a +
    fromIntegral (py bButton) * b .==
    fromIntegral (py prize + offset)
  minimize "cost" $ 3 * a + b

part1 :: Input -> IO Integer
part1 machines = do
  costs <- mapConcurrently (findMinimumCost 0) machines
  pure $ sum $ catMaybes costs

part2 :: Input -> IO Integer
part2 machines = do
  costs <- mapConcurrently (findMinimumCost 10000000000000) machines
  pure $ sum $ catMaybes costs


-- main

example :: String
example = "\
\Button A: X+94, Y+34\n\
\Button B: X+22, Y+67\n\
\Prize: X=8400, Y=5400\n\
\\n\
\Button A: X+26, Y+66\n\
\Button B: X+67, Y+21\n\
\Prize: X=12748, Y=12176\n\
\\n\
\Button A: X+17, Y+86\n\
\Button B: X+84, Y+37\n\
\Prize: X=7870, Y=6450\n\
\\n\
\Button A: X+69, Y+23\n\
\Button B: X+27, Y+71\n\
\Prize: X=18641, Y=10279"

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print =<< part1 testInput
  print =<< part1 realInput
  putStrLn "# Part 2"
  print =<< part2 testInput
  print =<< part2 realInput

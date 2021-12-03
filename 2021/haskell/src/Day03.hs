-- import

import           Control.Monad
import           Data.Char
import           Data.Function    (on)
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC


-- input

type Bit     = Int
type Reading = [Bit]
type Input   = [Reading]

parseInput :: String -> Input
parseInput = map (map digitToInt) . lines


-- solution

gammaEpsilon :: Input -> (Reading, Reading)
gammaEpsilon (transpose -> readings) = (gamma, epsilon)
  where
    gamma   = map mostCommon readings
    epsilon = map (1-) gamma
    mostCommon l =
      let (zeroes, ones) = span (== 0) $ sort l
      in  fromEnum (length ones >= length zeroes)

part1 :: Input -> Int
part1 readings = toDecimal gamma * toDecimal epsilon
  where
    (gamma, epsilon) = gammaEpsilon readings
    toDecimal = foldl' (\a d -> a*2 + d) 0

part2 :: Input -> Int
part2 readings = toDecimal oxygen * toDecimal co2
  where
    oxygen = identify fst 0 readings
    co2    = identify snd 0 readings

    toDecimal = foldl' (\a d -> a*2 + d) 0

    identify
      :: ((Reading, Reading) -> Reading) -- which of gamma / epsilon?
      -> Int                             -- index
      -> [Reading]                       -- readings
      -> Reading
    identify which index = \case
      []  -> error "whoops filtered too much I guess"
      [x] -> x
      l   -> let reference = which $ gammaEpsilon l
             in  identify which (index+1) $ filterOn index reference l

    filterOn :: Int -> Reading -> [Reading] -> [Reading]
    filterOn index reference = filter $
      \bits -> bits !! index == reference !! index


-- main

main :: IO ()
main = aocMain 3 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  -- mapM_ print testInput
  -- mapM_ print $ transpose testInput
  -- mapM_ print $ map mostCommon $ transpose testInput
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

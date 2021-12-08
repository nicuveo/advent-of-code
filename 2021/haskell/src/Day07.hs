-- import

import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Function               (on)
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC


-- input

type Input = [Int]

parseInput :: String -> Input
parseInput = parseWith $ number `sepBy` symbol ","


-- solution

part1 :: Input -> Int
part1 crabs = minimum $ parMap rseq fuel [minX .. maxX]
  where
    minX = minimum crabs
    maxX = maximum crabs
    fuel x = sum $ map (abs . subtract x) crabs

part2 :: Input -> Int
part2 crabs = minimum $ parMap rseq fuel [minX .. maxX]
  where
    minX = minimum crabs
    maxX = maximum crabs
    fuel x = sum $ map (triangular . abs . subtract x) crabs
    triangular x = (x * (x+1)) `div` 2


-- main

main :: IO ()
main = aocMain 7 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "16,1,2,0,4,2,7,1,2,14"

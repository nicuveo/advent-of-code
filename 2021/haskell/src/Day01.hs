-- import

import           Control.Monad
import           Data.Function    (on)
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC


-- input

type Input = [Int]

parseInput :: String -> Input
parseInput = map read . lines


-- solution

part1 :: Input -> Int
part1 [] = 0
part1 l  = countTrue $ zipWith (<) l (tail l)

part2 :: Input -> Int
part2 (x:a:b:y:l)
  | y > x     = 1 + part2 (a:b:y:l)
  | otherwise =     part2 (a:b:y:l)
part2 _ = 0


-- main

main :: IO ()
main = aocMain 01 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263"

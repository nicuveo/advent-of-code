-- import

import           Control.Monad
import           Data.Function (on)
import qualified Data.IntSet   as S
import           Data.List
import           Data.Maybe
import           Text.Parsec

import           AOC



-- input

type Input = [Int]

parseInput :: String -> Input
parseInput = map read . lines



-- solution

part1 :: Input -> Int
part1 input = head $ do
  n <- input
  guard $ S.member (2020 - n) inputSet
  return $ n * (2020 - n)
  where inputSet = S.fromList input

part2 :: Input -> Int
part2 input = head $ do
  (first:rest) <- tails input
  second       <- rest
  guard $ first + second < 2020
  guard $ S.member (2020 - first - second) inputSet
  return $ first * second * (2020 - first - second)
  where inputSet = S.fromList input



-- main

main :: IO ()
main = aocMain 1 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Given example"
  print $ part1 testInput
  print $ part2 testInput -- 241861950
  putStrLn "# Real input"
  print $ part1 realInput
  print $ part2 realInput

example :: String
example = "1721\n979\n366\n299\n675\n1456"

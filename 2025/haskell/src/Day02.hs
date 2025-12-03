module Day02 where

import "this" Prelude

import AOC

import Control.Parallel.Strategies
import Data.List.Extra             (nubOrd)
import Text.Parsec


type Input = [(Int, Int)]

parseInput :: String -> Input
parseInput = parseWith (range `sepBy` symbol ",")
  where
    range = do
      begin <- number
      symbol "-"
      end <-number
      pure (begin, end)


nearestEvenIDSize :: Int -> Int
nearestEvenIDSize = getIDSize
  >>> (+1)
  >>> (`div` 2)
  >>> (*2)

getIDSize :: Int -> Int
getIDSize = (+1)
  >>> fromIntegral
  >>> logBase 10
  >>> (ceiling @Double)

part1 :: Input -> Int
part1 = sum . parMap rpar findInvalid
  where
    findInvalid (begin, end) = sum do
      let sizeBegin = nearestEvenIDSize begin
          sizeEnd   = nearestEvenIDSize end
      size <- [sizeBegin,sizeBegin+2..sizeEnd]
      let segmentSize = size `div` 2
      segment <- [10^(segmentSize-1)..(10^segmentSize)-1]
      let candidate = segment + segment * (10 ^ segmentSize)
      guard $ candidate >= begin && candidate <= end
      pure candidate

part2 :: Input -> Int
part2 = sum . parMap rpar (sum . findInvalid)
  where
    findInvalid (begin, end) = nubOrd do
      let sizeBegin = getIDSize begin
          sizeEnd   = getIDSize end
      size <- [sizeBegin..sizeEnd]
      segmentSize <- [1..size-1]
      let (segments, remainder) = size `divMod` segmentSize
      guard $ remainder == 0
      segment <- [10^(segmentSize-1)..(10^segmentSize)-1]
      let candidate = foldr ($) 0
            $ replicate segments
            $ \n -> segment + n * 10^segmentSize
      guard $ candidate >= begin && candidate <= end
      pure candidate

example :: String
example = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

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

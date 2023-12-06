module Day06 where


-- import

import AOC
import "this" Prelude

import Text.Parsec


-- input

type Input = [Race]
type Race  = (Int, Int)

parseInput :: String -> Input
parseInput = parseWith do
  times     <- symbol "Time:"     >> many1 number
  distances <- symbol "Distance:" >> many1 number
  pure $ zip times distances


-- solution

findZeroes :: Race -> (Double, Double)
findZeroes (fromIntegral -> b, fromIntegral -> c) =
  if z1 > z2 then (z2, z1) else (z1, z2)
  where
    z1 = -(-b + sqrt (b*b - 4*c)) / 2
    z2 = -(-b - sqrt (b*b - 4*c)) / 2

possibilities :: Race -> Int
possibilities r = z2 - z1 + 1 - checkZero r z1 - checkZero r z2
  where
    (ceiling -> z1, floor -> z2) = findZeroes r
    checkZero (time, distance) x =
      if (time - x) * x == distance then 1 else 0

combine :: [Int] -> Int
combine = read . concatMap show

part1 :: Input -> Int
part1 = product . map possibilities

part2 :: Input -> Int
part2 = possibilities . bimap combine combine . unzip


-- main

example :: String
example = "Time:      7  15   30\nDistance:  9  40  200"

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

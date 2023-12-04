module Day01 where


-- import

import AOC
import "this" Prelude

import Data.Char
import Data.List      (tails)


-- input

type Input = [String]

parseInput :: String -> Input
parseInput = lines


-- solution

part1 :: Input -> Int
part1 = sum . map toNumber
  where
    toNumber s = let s' = filter isDigit s in read [head s', last s']

part2 :: Input -> Int
part2 = sum . map toNumber
  where
    toNumber s =
      let digits = mapMaybe identifyDigit $ tails s
          firstDigit = head digits
          lastDigit  = last digits
      in 10 * firstDigit + lastDigit
    identifyDigit s
      | "one"   == take 3 s = Just 1
      | "two"   == take 3 s = Just 2
      | "three" == take 5 s = Just 3
      | "four"  == take 4 s = Just 4
      | "five"  == take 4 s = Just 5
      | "six"   == take 3 s = Just 6
      | "seven" == take 5 s = Just 7
      | "eight" == take 5 s = Just 8
      | "nine"  == take 4 s = Just 9
      | otherwise           = case s of
          (c:_) | isDigit c -> Just $ digitToInt c
          _                 -> Nothing


-- main

example1 :: String
example1 = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

example2 :: String
example2 = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"

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

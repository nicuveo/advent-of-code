module Main where


-- import

import Data.List (nub, tails)

import AOC


-- input

type Input = String



-- solution

part1 :: Input -> Int
part1 = fst . head . filter (((==) =<< nub) . snd) . zip [4..] . map (take 4) . tails

part2 :: Input -> Int
part2 = fst . head . filter (((==) =<< nub) . snd) . zip [14..] . map (take 14) . tails


-- main

main :: IO ()
main = aocMain 06 $ \rawData -> do
  let testInput = example
      realInput = rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

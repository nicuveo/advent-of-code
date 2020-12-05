-- import

import           Control.Monad
import           Data.Function    (on)
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC



-- input

type Input = String

parseInput :: String -> Input
parseInput = parseLinesWith line
  where line = undefined



-- solution

part1 :: Input -> String
part1 = undefined

part2 :: Input -> String
part2 = undefined



-- main

main :: IO ()
main = aocMain 00 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = ""

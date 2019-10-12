-- import

import           Data.Function (on)
import           Data.List
import           Data.Maybe
import           Text.Parsec

import           AOC



-- input

type Input = String

parseInput :: String -> Input
parseInput = map parseLine . lines
  where parseLine = parseWith line
        line = undefined



-- solution

part1 :: Input -> String
part1 = undefined
  where f = undefined

part2 :: Input -> String
part2 = undefined
  where f = undefined



-- main

main :: IO ()
main = aocMain 23 $ \rawInput -> do
  let input = parseInput rawInput
  print $ part1 input
  print $ part2 input

-- import

import           Control.Monad
import           Data.Function    (on)
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC



-- input

type Input = (Int, Int)

parseInput :: String -> Input
parseInput = toPair . take 2 . map read . lines
  where toPair [a,b] = (a,b)
        toPair _     = error "wrong numer of input lines"

step :: Int -> Int -> Int
step sn x = mod (sn * x) 20201227



-- solution

iterations :: Int -> [Int]
iterations sn = iterate (step sn) 1

findLoopSize :: Int -> Int
findLoopSize x = head $ elemIndices x $ iterations 7

part1 :: Input -> Int
part1 (cardPK, doorPK) = iterations cardPK !! findLoopSize doorPK



-- main

main :: IO ()
main = aocMain 25 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput

example :: String
example = "5764801\n17807724"

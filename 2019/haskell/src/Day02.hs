{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}



-- import

import           Data.List.Split
import qualified Data.Vector.Unboxed as V

import           AOC
import           IntCode



-- input

type Input = [Int]

parseInput :: String -> Input
parseInput = map read . splitOn ","



-- solution

part1 :: Input -> Int
part1 input = run program V.! 0
  where program = V.fromList input V.// [(1, 12), (2, 2)]

part2 :: Input -> Int
part2 input = head [ 100 * n + v
                   | n <- [0..99]
                   , v <- [0..99]
                   , answer n v == 19690720
                   ]
  where answer n v = run (V.fromList input V.// [(1, n), (2, v)]) V.! 0



-- main

main :: IO ()
main = aocMain 2 $ \rawInput -> do
  let input = parseInput rawInput
  print $ part1 input
  print $ part2 input

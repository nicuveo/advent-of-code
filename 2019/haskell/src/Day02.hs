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

exec :: Int -> Int -> [Int] -> Int
exec i1 i2 v = fst (run program []) V.! 0
  where program = V.fromList v V.// [(1, i1), (2, i2)]


part1 :: Input -> Int
part1 = exec 12 2

part2 :: Input -> Int
part2 input = head [ 100 * n + v
                   | n <- [0..99]
                   , v <- [0..99]
                   , answer n v == 19690720
                   ]
  where answer n v = exec n v input



-- main

main :: IO ()
main = aocMain 2 $ \rawInput -> do
  let input = parseInput rawInput
  print $ part1 input
  print $ part2 input

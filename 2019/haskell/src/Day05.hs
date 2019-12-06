-- import

import           Data.List.Split
import qualified Data.Vector.Unboxed as V

import           AOC
import           IntCode



-- input

type Input = V.Vector Int

parseInput :: String -> Input
parseInput = V.fromList . map read . splitOn ","



-- solution

exec :: Int -> Input -> [Int]
exec x program = snd $ run program [x]


part1 :: Input -> [Int]
part1 = exec 1

part2 :: Input -> [Int]
part2 = exec 5



-- main

main :: IO ()
main = aocMain 5 $ \rawInput -> do
  let input = parseInput rawInput
  print $ part1 input
  print $ part2 input

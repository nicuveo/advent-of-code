-- import

import           AOC



-- input

type Input = [Int]

parseInput :: String -> Input
parseInput = map read . lines



-- solution

part1 :: Input -> Int
part1 = sum . map fuelRequired

part2 :: Input -> Int
part2 = sum . map (sum . takeWhile (>0) . tail . iterate fuelRequired)

fuelRequired :: Int -> Int
fuelRequired mass = max 0 $ div mass 3 - 2



-- main

main :: IO ()
main = aocMain 1 $ \rawInput -> do
  let input = parseInput rawInput
  print $ part1 input
  print $ part2 input

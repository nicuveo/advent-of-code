-- import

import qualified Data.Vector.Unboxed as V

import           AOC
import           IntCode
import           IntCodePlusPlus



-- input

type Input = [Int]

parseInput :: String -> Input
parseInput = map read . lines



-- intcode++

program :: String
program = unlines [ "read mass"
                  , "total = 0"
                  , "while (mass >= 0) {"
                  , "  total = total + mass / 3 - 2"
                  , "  read mass"
                  , "}"
                  , "print total"
                  ]



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
  print $ parseProgram "day01" program
  print $ compile =<< parseProgram "day01" program
  let ic = either error id $ transpile "day01" program
  print ic
  print $ run (V.fromList ic) (input ++ [-1])

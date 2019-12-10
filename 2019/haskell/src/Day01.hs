{-# LANGUAGE QuasiQuotes #-}

-- import

import           Data.String.QQ
import qualified Data.Vector.Unboxed as V

import           AOC
import           IntCode
import           IntCodeMeta
import           IntCodePlusPlus



-- input

type Input = [Int]

parseInput :: String -> Input
parseInput = map read . lines



-- intcode++

program :: String

program = [s|
/*
  index = 6
  print @8
  @8 = 42
  print @8
  print $index
  $index = 42
  print $index
  print &index
  print index
*/

  read remaining
  total = 0
  while (remaining > 0) {
    read mass
    total = total + mass / 3 - 2
    remaining = remaining - 1
  }
  print total
|]


-- solution

part1 :: Input -> Int
part1 = sum . map fuelRequired

part2 :: Input -> Int
part2 = sum . map (sum . takeWhile (>0) . tail . iterate fuelRequired)

fuelRequired :: Int -> Int
fuelRequired mass = max 0 $ div mass 3 - 2



-- main

runWithInput :: [Int] -> [[Int]] -> [Int]
runWithInput prog inputs = snd $ run (V.fromList prog) $ prefixLength =<< inputs
  where prefixLength l = length l : l

main :: IO ()
main = aocMain 1 $ \rawInput -> do
  let input = parseInput rawInput
  print $ part1 input
  print $ part2 input
  putStrLn "======"
  putStrLn ""

  let interpreter   = makeInterpreter
      day01         = either error id $ transpile "day01" program
      fakeInput     = [23,5,7]
  print $ length interpreter
  print $ runWithInput day01       [fakeInput]
  print $ runWithInput interpreter [day01, fakeInput]
  print $ runWithInput interpreter [interpreter, day01, fakeInput]
  print $ runWithInput interpreter [interpreter, interpreter, day01, fakeInput]

-- import

import           Data.List.Split
import qualified Data.Vector.Unboxed as V

import           AOC
import           IntCode



-- input

type Program = V.Vector Int

parseProgram :: String -> Program
parseProgram = V.fromList . map read . splitOn ","



-- solution

exec :: Program -> [Int] -> [Int]
exec = snd ... run


part1 :: Program -> [Int]
part1 = flip exec [1]

part2 :: Program -> Int
part2 = undefined



-- main

main :: IO ()
main = aocMain 9 $ \rawProgram -> do
  let input = parseProgram rawProgram
  print $ part1 input
  print $ part2 input

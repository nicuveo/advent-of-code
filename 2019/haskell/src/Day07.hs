-- import

import           Data.List
import           Data.List.Split
import qualified Data.Vector         as BV
import qualified Data.Vector.Unboxed as V

import           AOC
import           IntCode



-- input

type Input = V.Vector Int

parseInput :: String -> Input
parseInput = V.fromList . map read . splitOn ","



-- solution

part1 :: Input -> Int
part1 program = maximum $ map computeOutput $ permutations [0,1,2,3,4]
  where computeOutput = foldl executeOnce 0
        executeOnce signal ps = head $ snd $ run program [ps, signal]


part2 :: Input -> Int
part2 program = maximum $ map computeOutput $ permutations [5,6,7,8,9]
  where computeOutput (p:ps) =
          runConcurrently program $ BV.fromList $ [p, 0] : map pure ps
        computeOutput _ = error "wat"



-- main

main :: IO ()
main = aocMain 7 $ \rawInput -> do
  let input = parseInput rawInput
  print $ part1 input
  print $ part2 input

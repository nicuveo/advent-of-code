{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ParallelListComp #-}


-- import

import           Control.Parallel.Strategies
import           Data.Char                   (digitToInt)
import           Data.List

import           AOC



-- input

type Signal = [Int]

parseInput :: String -> Signal
parseInput = map digitToInt



-- solution

truncatedDigitPattern :: Int -> [Int]
truncatedDigitPattern = (patterns !!)
  where patterns    = map (tail . mkPattern) [1..]
        mkPattern n = cycle $ [1,0,-1,0] >>= replicate n

step :: Signal -> Signal
step s = withStrategy (parList rseq)
  [ flip mod 10 $ abs $ a + sum (zipWith (*) b $ truncatedDigitPattern i)
  | i <- [0..]
  | a:b <- tails s
  ]

showSignal :: Int -> Signal -> String
showSignal = concatMap show ... take

part1 :: Signal -> String
part1 = showSignal 8 . (!! 100) . iterate step

part2 :: Signal -> String
part2 = showSignal 8 . run 100
  where run  0 = id
        run !n = run (n-1) . scanr1 (\ !a !b -> mod (a+b) 10)



-- main

main :: IO ()
main = aocMain 16 $ \rawInput -> do
  let input  = parseInput rawInput
      offset = read $ showSignal 7 input
      signal = drop offset $ concat $ replicate 10000 input

  putStrLn $ "Part1: " ++ part1 input
  putStrLn $ "Part2: " ++ part2 signal

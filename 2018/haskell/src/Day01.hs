-- module

module Day01 (day01_1, day01_2) where



-- import

import           Common
import           Data.List
import           Text.Parsec



-- solution

day01_1 :: Solution
day01_1 = show . sum . parseInput


day01_2 :: Solution
day01_2 input = show $ findFirstDuplicate [] frequencies
  where changes     = parseInput input
        frequencies = scanl (+) 0 $ cycle changes
        findFirstDuplicate history (f : fs)
          | f `elem` history = f
          | otherwise        = findFirstDuplicate (f : history) fs
        findFirstDuplicate _ [] = error "not reachable"

-- day01_2 input = show $ take 10 $ sort [ (i, div (a - b) delta, a)
--                                       | (i, a) <- zip [0..] frequencies
--                                       , b <- frequencies
--                                       , a > b
--                                       , mod (a - b) delta == 0
--                                       ]
--   where changes     = parseInput input
--         frequencies = scanl (+) 0 changes
--         delta       = sum changes


-- helpers

parseInput :: String -> [Int]
parseInput = parseWith $ number `sepBy` newline
  where number = do
          sign <- tryAll [symbol "+", symbol "-"]
          n    <- intParser
          return $ if sign == "+" then n else -n

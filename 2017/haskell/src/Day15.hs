-- module

module Day15 (day15_1, day15_2) where



-- import

import           Data.Bits
import           Text.Parsec

import           Common



-- solution

day15_1 :: Solution
day15_1 input = show $ countTrue $ zipWith (==) valsA valsB
  where [startValueA, startValueB] = parseStartValue <$> lines input
        valsA = generate maxIter1 1 genAFactor startValueA
        valsB = generate maxIter1 1 genBFactor startValueB


day15_2 :: Solution
day15_2 input = show $ countTrue $ zipWith (==) valsA valsB
  where [startValueA, startValueB] = parseStartValue <$> lines input
        valsA = generate maxIter2 4 genAFactor startValueA
        valsB = generate maxIter2 8 genBFactor startValueB



-- helpers

genAFactor, genBFactor, divider, maxIter1, maxIter2 :: Int
genAFactor =      16807
genBFactor =      48271
divider    = 2147483647
maxIter1   =   40000000
maxIter2   =    5000000

simplify :: Int -> Int
simplify x = x .&. 0xFFFF

generate :: Int -> Int -> Int -> Int -> [Int]
generate iter cmod factor start =
  map simplify $ take iter $ filter (isMod cmod) $ drop 1 $ iterate step start
  where step val  = mod (factor * val) divider
        isMod m x = mod x m == 0

parseStartValue :: String -> Int
parseStartValue = parseWith line
  where line = do
          string "Generator "
          anyChar
          string " starts with "
          intParser

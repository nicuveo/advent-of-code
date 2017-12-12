-- module

module Day02 (day02_1, day02_2) where



-- import

import           Control.Monad
import           Data.List

import           Common



-- solution

day02_1 :: Solution
day02_1 s = show $ sum [maxVal - minVal | (minVal, maxVal) <- map minMax $ readSpreadsheet s]
--                     [maximum line - minimum line | line <- readSpreadsheet s]

day02_2 :: Solution
day02_2 s = show $ sum $ do
  line <- readSpreadsheet s
  do
    a <- line
    b <- line
    guard $ a > b && a `mod` b == 0
    return $ a `div` b



-- helpers

readSpreadsheet :: String -> [[Int]]
readSpreadsheet s = [map read $ words line | line <- lines s]
--                = map read . words <$> lines s

minMax :: (Ord a, Bounded a) => [a] -> (a, a)
minMax = foldl' step accum
  where accum = (maxBound, minBound)
        step (minSoFar, maxSoFar) x = (min minSoFar x, max maxSoFar x)

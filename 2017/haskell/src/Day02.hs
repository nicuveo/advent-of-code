-- module

module Day02 (day02_1, day02_2) where



-- import

import           Common
import           Control.Monad
import           Data.Char
import           Data.List



-- solution

day02_1 :: Solution
day02_1 s = show $ sum [ maxVal - minVal | (minVal, maxVal) <- map minMax $ readSpreadsheet s ]
--                     [ maximum line - minimum line | line <- readSpreadsheet s ]

day02_2 :: Solution
day02_2 s = show $ sum $ do
  line <- readSpreadsheet s
  take 1 $ do
    a <- line
    b <- line
    guard $ a > b && a `mod` b == 0
    return $ a `div` b



-- helpers

readSpreadsheet :: String -> [[Int]]
readSpreadsheet s = [ map read $ words line | line <- lines s ]
--                = map read . words <$> lines s

minMax :: (Ord a, Bounded a) => [a] -> (a, a)
minMax = foldl' step accum
  where accum = (maxBound, minBound)
        step (minSoFar, maxSoFar) x = (min minSoFar x, max maxSoFar x)
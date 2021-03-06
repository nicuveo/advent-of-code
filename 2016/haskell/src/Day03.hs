-- module

module Day03 (day03_1, day03_2) where



-- import

import           Control.Monad
import           Data.List
import           Data.List.Split

import           Common



-- solution

day03_1 :: Solution
day03_1 input = show $ sum [1 | [a,b,c] <- triangles, a + b > c]
  where triangles = sort . map readInt . words <$> lines input

day03_2 :: Solution
day03_2 input = show $ sum [1 | [a,b,c] <- triangles, a + b > c]
  where triangles = map sort $ chunksOf 3 $ join $ transpose $ map readInt . words <$> lines input

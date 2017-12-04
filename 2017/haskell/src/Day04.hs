-- module

module Day04 (day04_1, day04_2) where



-- import

import           Data.List

import           Common



-- solution

day04_1 :: Solution
day04_1 input = show $ sum [ 1 | l <- words <$> lines input, l == nub l ]


day04_2 :: Solution
day04_2 input = show $ sum [ 1 | l <- map sort . words <$> lines input, l == nub l ]

-- module

module Day01 (day01_1, day01_2) where



-- import

import           Data.Char

import           Common



-- solution

day01_1 :: Solution
day01_1 [] = error "day01_1: malformed input"
day01_1 l1@(x:xs) = show $ sum [ digitToInt a | (a,b) <- zip l1 l2, a == b ]
  where l2 = xs ++ [x]

day01_2 :: Solution
day01_2 l = show $ sum [ 2 * digitToInt a | (a, b) <- zip l1 l2, a == b ]
  where (l1, l2) = splitAt (length l `div` 2) l

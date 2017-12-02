-- module

module Day1 (day1_1, day1_2) where



-- import

import           Common
import           Data.Char



-- solution

day1_1 :: Solution
day1_1 l1@(x:xs) = show $ sum [ digitToInt a | (a,b) <- zip l1 l2, a == b ]
  where l2 = xs ++ [x]

day1_2 :: Solution
day1_2 l = show $ sum [ 2 * digitToInt a | (a, b) <- zip l1 l2, a == b ]
  where (l1, l2) = splitAt (length l `div` 2) l

-- module

module Day1 (day1) where



-- import

import           Common
import           Data.Char



-- solution

day1 :: Solution
day1 l1@(x:xs) = show $ sum [ digitToInt a | (a,b) <- zip l1 l2, a == b ]
  where l2 = xs ++ [x]

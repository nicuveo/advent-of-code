-- module

module Day04 (day04_1, day04_2) where



-- import

import           Data.List

import           Common



-- solution

day04_1 :: Solution
day04_1 = show . countIf check . map words . lines


day04_2 :: Solution
day04_2 = show . countIf check . map (map sort . words) . lines



-- helpers

check :: Eq a => [a] -> Bool
check []     = True
check (x:xs) = notElem x xs && check xs
-- check l = l == nub l

-- module

module Day04 (day04_1, day04_2) where



-- import

import           Data.List

import           Common



-- solution

day04_1 :: Solution
day04_1 pws = show $ sum [ 1 | l <- words <$> lines pws, check l ]


day04_2 :: Solution
day04_2 pws = show $ sum [ 1 | l <- map sort . words <$> lines pws, check l ]



-- helpers

check :: Eq a => [a] -> Bool
check []     = True
check (x:xs) = notElem x xs && check xs
-- check l = l == nub l

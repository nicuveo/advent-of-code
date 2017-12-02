-- module

module Day02 (day02_1, day02_2) where



-- import

import           Common
import           Control.Monad
import           Data.Bool
import           Data.Char
import           Data.List



-- solution

day02_1 :: Solution
day02_1 code = map intToDigit $ drop 1 $ scanl' (foldl' step1) 5 $ lines code

day02_2 :: Solution
day02_2 code = map digitAt $ drop 1 $ scanl' (foldl' step2) (3, 2) $ lines code



-- helpers

step1 x 'U' = bool x (x - 3) $ x > 3
step1 x 'D' = bool x (x + 3) $ x < 7
step1 x 'L' = bool x (x - 1) $ (x `mod` 3) /= 1
step1 x 'R' = bool x (x + 1) $ (x `mod` 3) /= 0

step2 old@(r, c) 'U' = let new = (r - 1, c) in bool old new $ digitAt new /= ' '
step2 old@(r, c) 'D' = let new = (r + 1, c) in bool old new $ digitAt new /= ' '
step2 old@(r, c) 'L' = let new = (r, c - 1) in bool old new $ digitAt new /= ' '
step2 old@(r, c) 'R' = let new = (r, c + 1) in bool old new $ digitAt new /= ' '

digitAt (r, c) = ["       ",
                  "   1   ",
                  "  234  ",
                  " 56789 ",
                  "  ABC  ",
                  "   D   ",
                  "       "] !! r !! c

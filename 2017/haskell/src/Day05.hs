-- module

module Day05 (day05_1, day05_2) where



-- import

import           Common

import           Data.Array



-- solution

day05_1 :: Solution
day05_1 = show . jump 0 0 . toArray . map readInt . lines
  where jump jumps inst steps
          | inst `within` bounds steps =
              let d = steps ! inst
              in  jump (jumps + 1) (inst + d) $ steps // [(inst, d + 1)]
          | otherwise = jumps

day05_2 :: Solution
day05_2 = show . jump 0 0 . toArray . map readInt . lines
  where jump jumps inst steps
          | inst `within` bounds steps =
              let d = steps ! inst
              in  if d > 2
                  then jump (jumps + 1) (inst + d) $ steps // [(inst, d - 1)]
                  else jump (jumps + 1) (inst + d) $ steps // [(inst, d + 1)]
          | otherwise = jumps



-- helpers

readInt :: String -> Int
readInt = read

toArray :: [a] -> Array Int a
toArray l = listArray (0, length l -1) l

within :: Ord a => a -> (a, a) -> Bool
x `within` (l, u) = l <= x && x <= u

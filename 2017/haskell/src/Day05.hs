{-# LANGUAGE BangPatterns #-}



-- module

module Day05 (day05_1, day05_2) where



-- import

import           Control.Monad.ST
import           Data.Array.ST

import           Common



-- solution

day05_1 :: Solution
day05_1 input = show $ runST $ jump update 0 0 =<< toArray (map readInt $ lines input)
  where update d = d + 1

day05_2 :: Solution
day05_2 input = show $ runST $ jump update 0 0 =<< toArray (map readInt $ lines input)
  where update d = if d > 2 then d - 1 else d + 1



-- helpers

type IntArray s = STUArray s Int Int

toArray :: [Int] -> ST s (IntArray s)
toArray l = newListArray (0, length l -1) l

within :: Ord a => a -> (a, a) -> Bool
x `within` (l, u) = l <= x && x <= u

jump :: (Int -> Int) -> Int -> Int -> IntArray s -> ST s Int
jump update !jumps !inst steps = do
  bs  <- getBounds steps
  if not $ inst `within` bs
    then return jumps
    else do
      d <- readArray steps inst
      writeArray steps inst $ update d
      jump update (jumps + 1) (inst + d) steps

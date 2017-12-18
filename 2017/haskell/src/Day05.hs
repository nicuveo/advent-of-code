{-# LANGUAGE BangPatterns #-}



-- module

module Day05 (day05_1, day05_2) where



-- import

import           Control.Monad.ST
import           Data.Vector.Unboxed         (fromList, thaw)
import           Data.Vector.Unboxed.Mutable as V

import           Common



-- solution

day05_1 :: Solution
day05_1 input = show $ runST $ jump update 0 0 =<< toArray (readInt <$> lines input)
  where update d = d + 1

day05_2 :: Solution
day05_2 input = show $ runST $ jump update 0 0 =<< toArray (readInt <$> lines input)
  where update d = if d > 2 then d - 1 else d + 1



-- helpers

type IntArray s = MVector s Int

toArray :: [Int] -> ST s (IntArray s)
toArray = thaw . fromList

jump :: (Int -> Int) -> Int -> Int -> IntArray s -> ST s Int
jump update !jumps !inst steps =
  if inst < 0 || inst >= V.length steps
  then return jumps
  else do
    d <- unsafeRead steps inst
    unsafeWrite steps inst $ update d
    jump update (jumps + 1) (inst + d) steps

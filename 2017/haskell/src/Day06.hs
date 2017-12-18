{-# LANGUAGE BangPatterns #-}



-- module

module Day06 (day06_1, day06_2) where



-- import

import           Control.Monad
import           Control.Monad.ST
import           Data.List                   as L
import           Data.Maybe
import           Data.Vector.Unboxed         as V
import           Data.Vector.Unboxed.Mutable as MV

import           Common



-- solution

day06_1 :: Solution
day06_1 input = show $ fst $ runST $ walk 0 [] =<< readBanks input

day06_2 :: Solution
day06_2 input = show $ snd $ runST $ walk 0 [] =<< readBanks input



-- helpers

type IntMArray s = MVector s Int

readBanks :: String -> ST s (IntMArray s)
readBanks = thaw . fromList . fmap readInt . words

walk :: Int -> [Vector Int] -> IntMArray s -> ST s (Int, Int)
walk !step hist banks = do
  blocks <- freeze banks
  case L.elemIndex blocks hist of
    Just n  -> return (step, n + 1)
    Nothing -> do
      let size   = V.length blocks
          maxVal = V.maximum blocks
          start  = fromJust $ V.elemIndex maxVal blocks
      unsafeWrite banks start 0
      redistribute banks maxVal size (start + 1)
      walk (step + 1) (blocks : hist) banks

redistribute :: IntMArray s -> Int -> Int -> Int -> ST s ()
redistribute banks left size x = when (left > 0) $ do
  let bank = x `mod` size
  current <- unsafeRead banks bank
  unsafeWrite banks bank (current + 1)
  redistribute banks (left - 1) size $ x + 1

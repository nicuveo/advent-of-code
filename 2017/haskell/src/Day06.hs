{-# LANGUAGE BangPatterns #-}



-- module

module Day06 (day06_1, day06_2) where



-- import

import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.List
import           Safe

import           Common



-- solution

day06_1 :: Solution
day06_1 input = show $ fst $ runST $ walk 0 [] =<< readBanks input

day06_2 :: Solution
day06_2 input = show $ snd $ runST $ walk 0 [] =<< readBanks input



-- helpers

type IntMArray s = STUArray s Int Int

toArray :: [Int] -> ST s (IntMArray s)
toArray l = newListArray (0, length l -1) l

readBanks :: String -> ST s (IntMArray s)
readBanks = toArray . map read . words

walk :: Int -> [Array Int Int] -> IntMArray s -> ST s (Int, Int)
walk !step hist banks = do
  this <- freeze banks
  case elemIndex this hist of
    Just n  -> return (step, n + 1)
    Nothing -> do
      let blocks = elems this
          size   = length blocks
          maxVal = maximum blocks
          start  = elemIndexJust maxVal blocks
      writeArray banks start 0
      redistribute banks maxVal size (start + 1)
      walk (step + 1) (this : hist) banks

redistribute :: IntMArray s -> Int -> Int -> Int -> ST s ()
redistribute banks left size x = when (left > 0) $ do
  let bank = x `mod` size
  current <- readArray banks bank
  writeArray banks bank (current + 1)
  redistribute banks (left - 1) size $ x + 1

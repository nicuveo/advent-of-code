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
day06_1 input = show $ runST $ walk1 0 [] =<< readBanks input
  where walk1 !step hist banks = do
          this <- freeze banks
          if this `elem` hist
            then return step
            else do
              let (size, maxVal, start) = extract this
              writeArray banks start 0
              redistribute banks maxVal size (start + 1)
              walk1 (step + 1) (this : hist) banks

day06_2 :: Solution
day06_2 input = show $ runST $ walk2 [] =<< readBanks input
  where walk2 hist banks = do
          this <- freeze banks
          case elemIndex this hist of
            Just n  -> return $ n + 1
            Nothing -> do
              let (size, maxVal, start) = extract this
              writeArray banks start 0
              redistribute banks maxVal size (start + 1)
              walk2 (this : hist) banks



-- helpers

type IntMArray s = STUArray s Int Int

toArray :: [Int] -> ST s (IntMArray s)
toArray l = newListArray (0, length l -1) l

readBanks :: String -> ST s (IntMArray s)
readBanks = toArray . map read . words

extract :: Array Int Int -> (Int, Int, Int)
extract banks = (length blocks, maxVal, elemIndexJust maxVal blocks)
  where blocks = elems banks
        maxVal = maximum blocks

redistribute :: IntMArray s -> Int -> Int -> Int -> ST s ()
redistribute banks left size x = when (left > 0) $ do
  let bank = x `mod` size
  current <- readArray banks bank
  writeArray banks bank (current + 1)
  redistribute banks (left - 1) size $ x + 1

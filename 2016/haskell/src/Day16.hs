{-# LANGUAGE BangPatterns #-}


-- module

module Day16 (day16_1, day16_2) where



-- import

import           Data.List
import           Data.List.Split (chunksOf)
import           Data.Maybe

import           Common



-- solution

day16_1 :: Solution
day16_1 = checksum . getData 272


day16_2 :: Solution
day16_2 = checksum . getData 35651584



-- helpers

step :: String -> String
step !s = s ++ '0' : map switch (reverse s)
  where switch '0' = '1'
        switch _   = '0'

getData :: Int -> String -> String
getData size start = take size $ fromJust $ find (\s -> length s >= size) $ iterate step start

checksum :: String -> String
checksum !current
  | even $ length current = checksum next
  | otherwise             = current
  where next = concat [ show $ fromEnum $ a == b
                      | [a,b] <- chunksOf 2 current
                      ]

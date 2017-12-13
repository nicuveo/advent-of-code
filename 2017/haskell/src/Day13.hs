{-# LANGUAGE BangPatterns #-}



-- module

module Day13 (day13_1, day13_2) where



-- import

import           Common

import           Text.Parsec



-- solution

day13_1 :: Solution
day13_1 = show . sum . map severity . filter (isCaught 0) . map parseLayer . lines

day13_2 :: Solution
day13_2 = show . delay 0 . map parseLayer . lines
  where delay !d l
          | any (isCaught d) l = delay (d+1) l
          | otherwise          = d



-- helpers

data Layer = Layer { depth :: Int
                   , range :: Int
                   } deriving (Show)

severity :: Layer -> Int
severity l = depth l * range l

isCaught :: Int -> Layer -> Bool
isCaught delay l = (depth l + delay) `mod` (2 * range l - 2) == 0

parseLayer :: String -> Layer
parseLayer = parseWith line
  where line = do
          d <- intParser
          string ": "
          r <- intParser
          return $ Layer d r

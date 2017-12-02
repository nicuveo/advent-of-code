-- module

module Day05 (day05_1, day05_2) where



-- import

import           Control.Monad
import           Data.Char
import           Data.Hash.MD5
import           Data.List
import           Data.List.Utils
import           Data.Maybe

import           Common



-- solution

day05_1 :: Solution
day05_1 did = take 8 [ c | i <- [0..], ('0':'0':'0':'0':'0':c:_) <- [hash did i] ]

day05_2 :: Solution
day05_2 did = map snd $ sort $ try [] 0
  where try pw i
          | length pw == 8 = pw
          | otherwise      = try (fromMaybe pw $ check pw i) $ i + 1
        check pw i = do
          let s       = hash did i
              (j:c:_) = drop 5 s
              k       = digitToInt j
          guard $ startswith "00000" s
          guard $ isDigit j
          guard $ 0 <= k && k < 8
          guard $ isNothing $ lookup k pw
          return $ (k, c) : pw



-- helpers

hash :: String -> Int -> String
hash did i = md5s $ Str $ did ++ show i

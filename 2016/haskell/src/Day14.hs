{-# LANGUAGE BangPatterns #-}



-- module

module Day14 (day14_1, day14_2) where



-- import

import           Data.List

import           Common



-- solution

day14_1 :: Solution
day14_1 salt = show $ findKeys 1 salt !! 63


day14_2 :: Solution
day14_2 salt = show $ findKeys 2017 salt !! 63



-- md5

type Salt = String

md5 :: Int -> Salt -> Int -> String
md5 count salt index = hashMD5s count $ salt ++ show index



-- keys

triplet :: String -> Maybe Char
triplet (a:b:c:s)
  | a == b && b == c = Just a
  | otherwise        = triplet $ b:c:s
triplet _ = Nothing

hasQuintuplet :: Char -> String -> Bool
hasQuintuplet c s = any ([c,c,c,c,c] `isPrefixOf`) $ tails s

findKeys :: Int -> Salt -> [Int]
findKeys count salt = findKeys_ 0 $ map (md5 count salt) [0..]
  where findKeys_ _      [] = error "findKeys: empty infinite list!?"
        findKeys_ !index (key:rest)
          | Just t <- triplet key,
            any (hasQuintuplet t) $ take 1000 rest = index : findKeys_ (index+1) rest
          | otherwise                              =         findKeys_ (index+1) rest

-- module

module Day10 (day10_1, day10_2) where



-- import

import           Data.Bits
import           Data.Char
import           Data.List
import           Data.List.Split
import           Text.Printf

import           Common



-- solution

day10_1 :: Solution
day10_1 = show . product . take 2 . sList . foldl' step start . map read . splitOn ","
  where start = State (indices 256) 0 0

day10_2 :: Solution
day10_2 = (printf "%02x" =<<) . map (foldl1' xor) . chunksOf 16 . sList . foldl' step start . concat . replicate 64 . (++ extra) . map ord
  where start = State (indices 256) 0 0
        extra = [17, 31, 73, 47, 23]



-- helpers

data State = State { sList  :: [Int]
                   , _sPos  :: Int
                   , _sSkip :: Int
                   }

instance Show State where
  show (State list pos _) = printf "[%s[%d]%s]" (lts a) x $ lts b
    where lts :: [Int] -> String
          lts [] = " "
          lts l  = printf " %s " $ unwords $ show <$> l
          (a,x:b) = splitAt pos list

indices :: Int -> [Int]
indices n = [[0..i-1] | i <- [0..]] !! n

wrap :: Int -> Int -> Int
wrap = flip mod

isWithin :: Int -> (Int, Int) -> Bool
x `isWithin` (a, b) = if a < b
                      then a <= x && x < b
                      else a <= x || x < b

step :: State -> Int -> State
step (State list pos skip) len = if len < 2
                                  then State list np ns
                                  else State nl   np ns
  where nl = [ list !! if i `isWithin` (pos, ww $ pos + len)
                       then ww $ 2 * pos + len - i - 1
                       else i
             | i <- indices ll
             ]
        ll = length list
        ww = wrap ll
        np = ww $ pos + len + skip
        ns = skip + 1

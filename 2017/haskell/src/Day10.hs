-- module

module Day10 (day10_1, day10_2) where



-- import

import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.List.Split
import qualified Data.Vector.Unboxed as V
import           Text.Printf

import           Common



-- solution

day10_1 :: Solution
day10_1 = show . V.product . V.take 2 . sVec . foldl' step start . map read . splitOn ","
  where start = State (indices 256) 0 0

day10_2 :: Solution
day10_2 = toHex <=< map (foldl1' xor) . chunksOf 16 . getList . foldl' step start . times 64 . (++ extra) . map ord
  where start = State (indices 256) 0 0
        extra = [17, 31, 73, 47, 23]
        toHex = printf "%02x"
        times = concat ... replicate



-- helpers

data State = State { sVec   :: V.Vector Int
                   , _sPos  :: Int
                   , _sSkip :: Int
                   }

instance Show State where
  show (State vec pos _) = printf "[%s[%d]%s]" (showL a) x $ showL b
    where showL :: [Int] -> String
          showL [] = " "
          showL l  = printf " %s " $ unwords $ show <$> l
          (a,x:b)  = splitAt pos $ V.toList vec

getList :: State -> [Int]
getList = V.toList . sVec

indices :: Int -> V.Vector Int
indices = V.enumFromN 0

isWithin :: Int -> (Int, Int) -> Bool
x `isWithin` (a, b) = if a < b
                      then a <= x && x < b
                      else a <= x || x < b

step :: State -> Int -> State
step (State vec pos skip) len = if len < 2
                                then State vec np ns
                                else State nv  np ns
  where nv = flip V.imap vec $ \i x ->  if i `isWithin` (pos, ww $ pos + len)
                                        then vec V.! ww (2 * pos + len - i - 1)
                                        else x
        ww = flip mod $ V.length vec
        np = ww $ pos + len + skip
        ns = skip + 1

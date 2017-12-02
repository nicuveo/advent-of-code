-- module

module Day01 (day01_1, day01_2) where



-- import

import           Common
import           Data.Char
import           Data.List



-- solution

day01_1 :: Solution
day01_1 path = show $ abs x + abs y
  where (x, y) = foldl' step (0, 0) $ words $ filter (/= ',') path
        step (x, y) ('R' : n) = (read n - y,  x)
        step (x, y) ('L' : n) = (read n + y, -x)

day01_2 :: Solution
day01_2 path = show $ abs x + abs y
  where (x, y) = walk $ snd $ foldl' step ((1, 0), [(0, 0)]) $ words $ filter (/= ',') path
        step (d, hist) ('R' : n) = (rotateR d, hist ++ generate (last hist) (rotateR d) (read n))
        step (d, hist) ('L' : n) = (rotateL d, hist ++ generate (last hist) (rotateL d) (read n))
        generate (x, y) (u, v) n = [(x + u * i, y + v * i) | i <- [1..n]]
        walk (p : ps)
          | p `elem` ps = p
          | otherwise   = walk ps



-- helpers

rotateR (u, v) = (v, -u)
rotateL (u, v) = (-v, u)

-- module

module Day01 (day01_1, day01_2) where



-- import

import           Data.List

import           Common



-- solution

day01_1 :: Solution
day01_1 path = show $ abs x + abs y
  where (x, y) = foldl' step1 (0, 0) $ words $ filter (/= ',') path


day01_2 :: Solution
day01_2 path = show $ abs x + abs y
  where (x, y) = walk $ snd $ foldl' step2 ((1, 0), [(0, 0)]) $ words $ filter (/= ',') path
        walk (p : ps)
          | p `elem` ps = p
          | otherwise   = walk ps
        walk _ = error "day01_1: no solution found"



-- helpers

type Vector = (Int, Int)

step1 :: Vector -> String -> Vector
step1 (x, y) ('R' : n) = (read n - y,  x)
step1 (x, y) ('L' : n) = (read n + y, -x)
step1 _ _              = error "day01_1: file format error"

step2 :: (Vector, [Vector]) -> String -> (Vector, [Vector])
step2 (d, hist) ('R' : n) = (rotateR d, hist ++ generate (last hist) (rotateR d) (read n))
step2 (d, hist) ('L' : n) = (rotateL d, hist ++ generate (last hist) (rotateL d) (read n))
step2 _ _                 = error "day01_2: file format error"

generate :: Vector -> Vector -> Int -> [Vector]
generate (x, y) (u, v) n = [(x + u * i, y + v * i) | i <- [1..n]]

rotateR, rotateL :: Vector -> Vector
rotateR (u, v) = (v, -u)
rotateL (u, v) = (-v, u)

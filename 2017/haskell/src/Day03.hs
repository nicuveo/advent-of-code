-- module

module Day03 (day03_1, day03_2, day03_tests) where



-- import

import           Safe
import           Test.QuickCheck

import           Common



-- solution

day03_1 :: Solution
day03_1 input = show $ abs x + abs y
  where (x, y) = getPos $ read input

day03_2 :: Solution
day03_2 input = show $ head [n | n <- getV2Val <$> [1..], n > value]
  where value = read input



-- helpers

type Pos = (Int, Int)

getPos :: Int -> Pos
getPos 1    = (0, 0)
getPos cell = case side of
               0 -> ( u,  v)
               1 -> (-v,  u)
               2 -> (-u, -v)
               3 -> ( v, -u)
               _ -> error "getPos: logic error?"
  where
    size  = head [n | n <- [1,3..], n*n >= cell]
    start = (size - 2) ^ 2 + 1
    side  = (cell - start) `div` (size - 1)
    dist  = (cell - start) `mod` (size - 1)
    u     = size `div` 2
    v     = dist - (size - 3) `div` 2

getVal :: Pos -> Int
getVal (0, 0) = 1
getVal (x, y)
  | abs x > abs y && x > 0 = val   x  0   y
  | abs x > abs y          = val (-x) 2 (-y)
  | y > 0                  = val   y  1 (-x)
  | otherwise              = val (-y) 3   x
  where
    val k s d = (2 * k - 1) ^ 2 + 1 + s * (2 * k) + d + k - 1

getV2Val :: Int -> Int
getV2Val 1 = 1
getV2Val n = at [val_ x $ getPos x | x <- [1..]] $ n - 1
  where val_ z (x,y) = sum [ getV2Val p
                           | p <- getVal <$> [(x-1,y+1),(x,y+1),(x+1,y+1),
                                              (x-1,y  ),        (x+1,y  ),
                                              (x-1,y-1),(x,y-1),(x+1,y-1)]
                           , p < z
                           ]



-- tests

day03_tests :: IO ()
day03_tests = quickCheck $ \ (Positive x) -> getVal (getPos x) == x

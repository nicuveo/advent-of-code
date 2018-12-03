-- module

module Day02 (day02_1, day02_2) where



-- import

import           Data.List

import           Common



-- solution

day02_1 :: Solution
day02_1 input  = show $ twos * threes
  where names  = sort <$> lines input
        twos   = length $ filter (containsNSimilar 2) names
        threes = length $ filter (containsNSimilar 3) names


day02_2 :: Solution
day02_2 input = head [ [a | (a,b) <- zip box1 box2, a == b]
                     | box1 <- names
                     , box2 <- names
                     , 1 == countTrue (zipWith (/=) box1 box2)
                     ]
  where names = lines input



-- helpers

containsNSimilar :: Int -> String -> Bool
containsNSimilar n s = any (\g -> length g == n) $ group s

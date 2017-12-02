-- module

module Day06 (day06_1, day06_2) where



-- import

import           Data.List

import           Common



-- solution

day06_1 :: Solution
day06_1 = map choose . transpose . lines
  where choose = head . last . sortOn length . group . sort


day06_2 :: Solution
day06_2 = map choose . transpose . lines
  where choose = head . head . sortOn length . group . sort

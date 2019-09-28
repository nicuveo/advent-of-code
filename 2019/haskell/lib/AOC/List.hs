-- module

module AOC.List where



-- import

import           Data.Function
import           Data.List

import           AOC.Misc



-- list helpers

countTrue :: [Bool] -> Int
countTrue = countIf id

countIf :: (a -> Bool) -> [a] -> Int
countIf = length ... filter

count :: Eq a => a -> [a] -> Int
count a = length . filter (a ==)

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn = minimumBy . on compare

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn = maximumBy . on compare

groupOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy . on (==)

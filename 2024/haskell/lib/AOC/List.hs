-- module

module AOC.List where


-- import

import Data.Function
import Data.List     (groupBy, maximumBy, minimumBy)


-- counting elements

countTrue :: [Bool] -> Int
countTrue = countIf id

countIf :: (a -> Bool) -> [a] -> Int
countIf predicate = length . filter predicate

count :: Eq a => a -> [a] -> Int
count a = length . filter (a ==)


-- on equivalents for by functions

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn = minimumBy . on compare

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn = maximumBy . on compare

groupOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy . on (==)


-- strictness operations

forceSpine :: [a] -> [a]
forceSpine = foldr (const id) () >>= seq

forceElements :: [a] -> [a]
forceElements = foldr seq () >>= seq

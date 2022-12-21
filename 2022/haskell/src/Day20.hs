{-# LANGUAGE ParallelListComp #-}

module Main where


-- import

import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as M
import Data.List           qualified as L
import Data.Maybe
import Text.Printf

import AOC

import Debug.Trace


-- input

type Input = [Int]
type Index = Int
type Value = Int

parseInput :: String -> Input
parseInput = map read . lines


-- solution

mix :: HashMap Index (Index, Value) -> HashMap Index (Index, Value)
mix m = L.foldl' step m [0..M.size m-1]
  where
    size = M.size m
    step :: HashMap Index (Index, Value) -> Int -> HashMap Index (Index, Value)
    step iMap ogIndex =
      let (currentIndex, value) = iMap ! ogIndex
          newIndex = (currentIndex + value) `mod` (size - 1)
      in  flip M.mapWithKey iMap \o (i,v) ->
        if | o == ogIndex     -> (newIndex, value)
           | i < currentIndex -> if i < newIndex then (i,v) else (i+1,v)
           | otherwise        -> if i > newIndex then (i,v) else (i-1,v)

mixN :: Int -> [Int] -> [Int]
mixN n = map snd . L.sort . M.elems . L.foldl1' (.) (replicate n mix) . M.fromList . zip [0..] . zip [0..]

part1 :: Input -> Int
part1 l = g 1000 + g 2000 + g 3000
  where r = mixN 1 l
        z = fromJust $ L.elemIndex 0 r
        g i = r !! mod (z+i) (length l)

part2 :: Input -> Int
part2 l = g 1000 + g 2000 + g 3000
  where r = mixN 10 $ map (*m) l
        m = 811589153
        z = fromJust $ L.elemIndex 0 r
        g i = r !! mod (z+i) (length l)


-- main

main :: IO ()
main = aocMain 20 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "1\n2\n-3\n3\n-2\n0\n4"

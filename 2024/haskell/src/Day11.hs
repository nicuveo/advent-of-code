{-# LANGUAGE TemplateHaskell #-}

module Day11 where


-- import

import AOC
import "this" Prelude

import Control.Lens
import Data.HashMap.Strict qualified as M


-- input

type Input = [Int]

parseInput :: String -> Input
parseInput = map read . words


-- solution

data Cache = Cache
  { _stoneMap  :: HashMap (Int, Int) Int
  , _cacheHit  :: Int
  , _cacheMiss :: Int
  }

makeLenses 'Cache

emptyCache :: Cache
emptyCache = Cache M.empty 0 0

numberOfDigits :: Int -> Int
numberOfDigits = succ . floor . (logBase 10 :: Double -> Double) . fromIntegral

project
  :: MonadState Cache m
  => Int
  -> Int
  -> m Int
project 0 _ = pure 1
project !n !stone = do
  uses stoneMap (M.lookup (stone, n)) >>= \case
    Just result -> do
      cacheHit += 1
      pure result
    Nothing     -> do
      cacheMiss += 1
      let ds = numberOfDigits stone
      result <- if
        | stone == 0 -> project (n-1) 1
        | odd ds     -> project (n-1) (stone * 2024)
        | otherwise  -> do
            let (stoneLeft, stoneRight) = stone `divMod` (10 ^ (ds `div` 2))
            rLeft  <- project (n-1) stoneLeft
            rRight <- project (n-1) stoneRight
            pure $ rLeft + rRight
      stoneMap %= M.insert (stone, n) result
      pure result

part1 :: Input -> IO Int
part1 stones = do
  (result, Cache s h m) <- flip runStateT emptyCache $ for stones (project 25)
  let hitRate = (fromIntegral h / fromIntegral (h+m)) :: Double
  putStrLn "Part 1:"
  putStrLn $ "  cache size:     " ++ show (M.size s)
  putStrLn $ "  cache calls:    " ++ show (h + m)
  putStrLn $ "  cache hit rate: " ++ show (hitRate * 100) ++ "%"
  pure $ sum result

part2 :: Input -> IO Int
part2 stones = do
  (result, Cache s h m) <- flip runStateT emptyCache $ for stones (project 75)
  let hitRate = (fromIntegral h / fromIntegral (h+m)) :: Double
  putStrLn "Part 2:"
  putStrLn $ "  cache size:     " ++ show (M.size s)
  putStrLn $ "  cache calls:    " ++ show (h + m)
  putStrLn $ "  cache hit rate: " ++ show (hitRate * 100) ++ "%"
  pure $ sum result


-- main

example :: String
example = "125 17"

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print =<< part1 testInput
  print =<< part1 realInput
  putStrLn "# Part 2"
  print =<< part2 testInput
  print =<< part2 realInput

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

-- import

import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Data.Function              (on)
import qualified Data.IntMap.Strict         as M
import           Data.List
import           Data.Maybe
import           Data.Traversable           (for)

import           AOC



-- input

type Input = [Int]

parseInput :: String -> Input
parseInput = map read . lines



-- solution

part1 :: Input -> Int
part1 input = count 3 diffs * count 1 diffs
  where adapters = (-3) : 0 : sort input
        diffs    = zipWith (-) (tail adapters) adapters

part2 :: Input -> Int
part2 input = flip evalState M.empty $ visit 0 adapters
  where adapters = 0 : sort input ++ [maximum input + 3]

type Cache = M.IntMap Int

visit :: (MonadState Cache m) => Int -> [Int] -> m Int
visit _ [] = return 0 -- no path
visit prev (current:adapters)
  | current - prev > 3 = return 0
  | null adapters      = return 1
  | otherwise          = do
      cache <- get
      case cache M.!? current of
        Just cached -> pure cached
        Nothing     -> do
          combinations <- traverse (visit current) $ take 3 $ tails adapters
          let result = sum combinations
          modify $ M.insert current result
          return result



-- pure version

purePart2 :: Input -> Int
purePart2 input = snd $ pureVisit M.empty 0 adapters
  where adapters = 0 : sort input ++ [maximum input + 3]

pureVisit :: Cache -> Int -> [Int] -> (Cache, Int)
pureVisit cache _ [] = (cache, 0)
pureVisit cache prev (current:adapters)
  | current - prev > 3 = (cache, 0)
  | null adapters      = (cache, 1)
  | otherwise          =
      case cache M.!? current of
        Just cached -> (cache, cached)
        Nothing     ->
          let step (c, r) ns = fmap (r+) $ pureVisit c current ns
              (cache', result) = foldl' step (cache, 0) $ take 3 $ tails adapters
              newCache = M.insert current result cache'
          in  (newCache, result)



-- version 2

part2b :: Input -> Int
part2b input = snd $ head $ foldr visit2b [] adapters
  where adapters = 0 : sort input ++ [maximum input + 3]

visit2b :: Int -> [(Int,Int)] -> [(Int,Int)]
visit2b current cache = (current,result) : take 2 cache
    where result = max 1 $ sum $ do
            (key, value) <- cache
            guard $ key - current <= 3
            pure value



-- golfed version

part1Golfed :: String -> Int
part1Golfed i=1#i*(1+3#i)
x#(sort.map read.lines->i)=sum[1|(a,b)<-zip(0:i)i,b-a==x]

part2Golfed :: String -> Int
part2Golfed(sort.map read.lines->i)=snd$foldr(\x c->(x,max 1$sum[b|(a,b)<-c,a-x<4]):c)[](0:i)!!0



-- main

main :: IO ()
main = aocMain 10 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 1 golfed"
  print $ part1Golfed example
  print $ part1Golfed rawData
  putStrLn "# Part 2: monads"
  print $ part2 testInput
  print $ part2 realInput
  putStrLn "# Part 2: pure"
  print $ purePart2 testInput
  print $ purePart2 realInput
  putStrLn "# Part 2: simple"
  print $ part2b testInput
  print $ part2b realInput
  putStrLn "# Part 2 golfed"
  print $ part2Golfed example
  print $ part2Golfed rawData

example :: String
example = "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3"

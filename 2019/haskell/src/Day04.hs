{-# LANGUAGE ViewPatterns #-}



-- import

import           Data.List
import           Data.List.Split

import           AOC



-- input

type Input = (Int, Int)

parseInput :: String -> Input
parseInput i = let [a,b] = read <$> splitOn "-" i in (a,b)



-- solution

part1 :: Input -> Int
part1 (a,b) = countIf (check (>1)) [a..b]

part2 :: Input -> Int
part2 (a,b) = countIf (check (==2)) [a..b]


check :: (Int -> Bool) -> Int -> Bool
check f (show -> n) =
  and (zipWith (<=) n $ tail n) && any (f . length) (group n)



-- main

main :: IO ()
main = aocMain 4 $ \rawInput -> do
  let input = parseInput rawInput
  print $ part1 input
  print $ part2 input

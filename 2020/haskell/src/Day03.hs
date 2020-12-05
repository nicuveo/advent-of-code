-- import

import           Control.Monad
import           Data.Function   (on)
import qualified Data.IntSet     as S
import           Data.List
import           Data.List.Split (chunksOf)
import           Data.Maybe

import           AOC



-- input

type Input = (Int, [S.IntSet])

parseInput :: String -> Input
parseInput input = (length $ head $ rows, mkSet <$> rows)
  where rows  = lines input
        mkSet = S.fromList . elemIndices '#'



-- solution

part1 :: Input -> Int
part1 (n, sets) = countTrue [S.member (column `mod` n) set | (column, set) <- zip [0,3..] sets]

part2 :: Input -> Int
part2 (n, sets) = product [ trees 1 1
                          , trees 1 3
                          , trees 1 5
                          , trees 1 7
                          , trees 2 1
                          ]
   where trees yOffset xOffset = countTrue $ do
           (column, set:_) <- zip [0,xOffset..] $ chunksOf yOffset sets
           pure $ S.member (column `mod` n) set



-- main

main :: IO ()
main = aocMain 3 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Given example"
  print $ part1 testInput
  print $ part2 testInput
  putStrLn "# Real input"
  print $ part1 realInput
  print $ part2 realInput

example :: String
example = "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#"

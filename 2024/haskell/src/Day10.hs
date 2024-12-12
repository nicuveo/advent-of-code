module Day10 where


-- import

import AOC
import "this" Prelude

import AOC.Grid.Flat
import Data.HashMap.Strict qualified as M
import Data.HashSet        qualified as S


-- input

type Input = Grid Char

parseInput :: String -> Input
parseInput = from2DList . lines


-- solution

getAll9s :: Input -> [Point]
getAll9s input = do
  p <- allPoints input
  guard $ (input ! p) == '9'
  pure p

part1 :: Input -> Int
part1 input = go '9' $ M.fromList do
  p <- getAll9s input
  pure (p, S.singleton p)
  where
    go '0'   reachable = sum $ fmap S.size $ M.elems reachable
    go stage reachable = go (pred stage) $ M.fromListWith S.union do
      (point, summits) <- M.toList reachable
      neighbour        <- gridFourSurroundingPoints input point
      guard $ (input ! neighbour) == pred stage
      pure (neighbour, summits)

part2 :: Input -> Int
part2 input = go '9' $ M.fromList do
  p <- getAll9s input
  pure (p, 1)
  where
    go '0'   reachable = sum $ M.elems reachable
    go stage reachable = go (pred stage) $ M.fromListWith (+) do
      (point, summits) <- M.toList reachable
      neighbour        <- gridFourSurroundingPoints input point
      guard $ (input ! neighbour) == pred stage
      pure (neighbour, summits)


-- main

example :: String
example = "\
\89010123\n\
\78121874\n\
\87430965\n\
\96549874\n\
\45678903\n\
\32019012\n\
\01329801\n\
\10456732"

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

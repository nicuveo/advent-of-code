module Day11 where


-- import

import AOC
import "this" Prelude

import Data.HashSet   qualified as S
import Data.List      (nub, tails)


-- input

data Input = Input
  { galaxies  :: HashSet Point
  , emptyRows :: HashSet Int
  , emptyCols :: HashSet Int
  }
  deriving Show

parseInput :: String -> Input
parseInput input = Input (S.fromList galaxies) (getHoles rows) (getHoles cols)
  where
    galaxies = concat $ zipWith getRowGalaxies [0..] (lines input)
    rows = nub $ sort $ map py galaxies
    cols = nub $ sort $ map px galaxies
    getHoles l = S.fromList $ concat $ zipWith inBetween l (tail l)
    getRowGalaxies y line = do
      (x, _) <- filter (\(_, c) -> c == '#') $ zip [0..] line
      pure $ Point x y
    inBetween a b = [a+1..b-1]


-- solution

distance :: Input -> Int -> Point -> Point -> Int
distance (Input _ emptyRows emptyCols) exps (Point x1 y1) (Point x2 y2) =
  (maxX - minX) + (maxY - minY) + expandedRows + expandedCols
  where
    minX = min x1 x2
    maxX = max x1 x2
    minY = min y1 y2
    maxY = max y1 y2
    expandedRows = sum do
      r <- S.toList emptyRows
      guard $ r > minY && r < maxY
      pure $ exps - 1
    expandedCols = sum do
      c <- S.toList emptyCols
      guard $ c > minX && c < maxX
      pure $ exps - 1

part1 :: Input -> Int
part1 input = sum do
  (g1:gs) <- tails $ S.toList $ galaxies input
  g2 <- gs
  pure $ distance input 2 g1 g2

part2 :: Input -> Int
part2 input = sum do
  (g1:gs) <- tails $ S.toList $ galaxies input
  g2 <- gs
  pure $ distance input 1000000 g1 g2


-- main

example :: String
example = "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#....."

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  print testInput
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

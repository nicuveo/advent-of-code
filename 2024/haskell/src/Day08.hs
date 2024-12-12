module Day08 where


-- import

import AOC
import "this" Prelude

import Data.HashMap.Strict qualified as M
import Data.HashSet        qualified as S


-- input

type Input = (Int, Int, HashMap Char [Point])

parseInput :: String -> Input
parseInput i = (length $ head $ lines i, length $ lines i, antennas)
  where
    antennas = M.fromListWith (<>) do
      (y, row) <- zip [0..] $ lines i
      (x, c)   <- zip [0..] row
      guard $ c /= '.'
      pure (c, [Point x y])


-- solution

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs (x:xs) = fmap (x,) xs ++ pairs xs

part1 :: Input -> Int
part1 (w, h, allAntennas) = S.size $ S.unions do
  antennaGroup <- M.elems allAntennas
  (a1, a2)     <- pairs antennaGroup
  result       <- [a2 + (a2 - a1), a1 + (a1 - a2)]
  guard $ px result >= 0 && px result < w && py result >= 0 && py result < h
  pure $ S.singleton result

part2 :: Input -> Int
part2 (w, h, allAntennas) = S.size $ S.unions do
  antennaGroup <- M.elems allAntennas
  (a1, a2)     <- pairs antennaGroup
  antinodes    <- [project a1 (a1 - a2), project a2 (a2 - a1)]
  pure $ S.fromList antinodes
  where
    project p v = takeWhile inBounds do
      n <- [0..]
      pure $ p + v *. n
    inBounds (Point x y) = x >= 0 && x < w && y >= 0 && y < h


-- main

example :: String
example = "\
\............\n\
\........0...\n\
\.....0......\n\
\.......0....\n\
\....0.......\n\
\......A.....\n\
\............\n\
\............\n\
\........A...\n\
\.........A..\n\
\............\n\
\............"

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

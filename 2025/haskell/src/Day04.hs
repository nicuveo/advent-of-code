module Day04 where

import "this" Prelude

import AOC
import AOC.Grid.Flat  as G


type Input = Grid Bool

parseInput :: String -> Input
parseInput = from2DList . map (map ('@' ==)) . lines


part1 :: Input -> Int
part1 g = countTrue do
  p <- allPoints g
  guard $ g ! p
  let n = countTrue $ gridEightNeighbours g p
  pure $ n < 4

part2 :: Input -> Int
part2 input = countRolls input - countRolls (go input)
  where
    go g =
      let g' = removeAccessibleRolls g
      in  if g /= g' then go g' else g'

countRolls :: Input -> Int
countRolls = countTrue . G.toList

removeAccessibleRolls :: Input -> Input
removeAccessibleRolls g = flip pmap g \p b ->
  b && countTrue (gridEightNeighbours g p) >= 4


example :: String
example = "\
  \..@@.@@@@.\n\
  \@@@.@.@.@@\n\
  \@@@@@.@.@@\n\
  \@.@@@@..@.\n\
  \@@.@@@@.@@\n\
  \.@@@@@@@.@\n\
  \.@.@.@.@@@\n\
  \@.@@@.@@@@\n\
  \.@@@@@@@@.\n\
  \@.@.@@@.@.\n"

run :: String -> IO ()
run rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

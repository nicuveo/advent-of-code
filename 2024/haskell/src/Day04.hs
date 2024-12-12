module Day04 where


-- import

import AOC
import AOC.Grid.Flat
import "this" Prelude


-- input

type Input = Grid Char

parseInput :: String -> Input
parseInput = from2DList . lines


-- solution

part1 :: Input -> Int
part1 grid = sum do
  p <- allPoints grid
  guard $ (grid ! p) == 'X'
  d <- [ Point (-1) 0
       , Point (-1) (-1)
       , Point    0 (-1)
       , Point    1 (-1)
       , Point    1    0
       , Point    1    1
       , Point    0    1
       , Point (-1)    1
       ]
  guard $ (grid !? (p + d *. 1)) == Just 'M'
  guard $ (grid !? (p + d *. 2)) == Just 'A'
  guard $ (grid !? (p + d *. 3)) == Just 'S'
  pure 1

part2 :: Input -> Int
part2 grid = sum do
  p <- allPoints grid
  guard $ (grid ! p) == 'A'
  let d1 = sort $ catMaybes [grid !? (p + Point (-1) (-1)), grid !? (p + Point 1 1)]
      d2 = sort $ catMaybes [grid !? (p + Point (-1) 1), grid !? (p + Point 1 (-1))]
  guard $ d1 == "MS"
  guard $ d2 == "MS"
  pure 1


-- main

example :: String
example = "\
\MMMSXXMASM\n\
\MSAMXMSMSA\n\
\AMXSXMAAMM\n\
\MSAMASMSMX\n\
\XMASAMXAMM\n\
\XXAMMXXAMA\n\
\SMSMSASXSS\n\
\SAXAMASAAA\n\
\MAMMMXMMMM\n\
\MXMXAXMASX"

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

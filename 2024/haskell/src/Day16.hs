module Day16 where


-- import

import AOC
import AOC.Grid.Flat
import "this" Prelude

import Data.HashSet   qualified as S
import Data.Maybe     (fromJust)
import Data.Monoid    (First (..))


-- input

type Input = (Point, Point, Grid Bool)

parseInput :: String -> Input
parseInput s =
  let g = from2DList (lines s)
      extractFirst = fromJust . getFirst . mconcat
      (start, end) = bimap extractFirst extractFirst $ unzip do
        p <- allPoints g
        case g ! p of
          'E' -> pure (First Nothing,  First (Just p))
          'S' -> pure (First (Just p), First Nothing)
          _   -> pure (First Nothing,  First Nothing)
  in (start, end, fmap (/= '#') g)


-- solution

neighbours :: Grid Bool -> (Point, Direction) -> [(Int, (Point, Direction))]
neighbours g (p, d) =
  let rotations = [(1000, (p, turn90L d)), (1000, (p, turn90R d))]
      newPoint  = p + directionVector d
  in
    if g ! newPoint
    then (1, (newPoint, d)) : rotations
    else rotations

part1 :: Input -> Int
part1 (start, end, grid) =
  fst $
  last $
  head $
  unsafeFindAnyPathWith (neighbours grid) heuristic (start, E) isEnd
  where
    isEnd (p, _) = p == end
    heuristic (p, _) = manhattanNorm $ end - p

part2 :: Input -> Int
part2 (start, end, grid) =
  S.size $
  S.unions $
  map toPointSet $
  unsafeFindAnyPathWith (neighbours grid) heuristic (start, E) isEnd
  where
    isEnd (p, _) = p == end
    heuristic = const 0
    toPointSet :: [(Int, (Point, Direction))] -> HashSet Point
    toPointSet = S.fromList . map (fst . snd)

pathSets :: Input -> HashSet Point
pathSets (start, end, grid) =
  S.unions $
  map toPointSet $
  unsafeFindAnyPathWith (neighbours grid) heuristic (start, E) isEnd
  where
    isEnd (p, _) = p == end
    heuristic = const 0
    toPointSet :: [(Int, (Point, Direction))] -> HashSet Point
    toPointSet = S.fromList . map (fst . snd)


-- main

example1 :: String
example1 = "\
\###############\n\
\#.......#....E#\n\
\#.#.###.#.###.#\n\
\#.....#.#...#.#\n\
\#.###.#####.#.#\n\
\#.#.#.......#.#\n\
\#.#.#####.###.#\n\
\#...........#.#\n\
\###.#.#####.#.#\n\
\#...#.....#.#.#\n\
\#.#.#.###.#.#.#\n\
\#.....#...#.#.#\n\
\#.###.#.#.#.#.#\n\
\#S..#.....#...#\n\
\###############"

example2 :: String
example2 = "\
\#################\n\
\#...#...#...#..E#\n\
\#.#.#.#.#.#.#.#.#\n\
\#.#.#.#...#...#.#\n\
\#.#.#.#.###.#.#.#\n\
\#...#.#.#.....#.#\n\
\#.#.#.#.#.#####.#\n\
\#.#...#.#.#.....#\n\
\#.#.#####.#.###.#\n\
\#.#.#.......#...#\n\
\#.#.###.#####.###\n\
\#.#.#...#.....#.#\n\
\#.#.#.#####.###.#\n\
\#.#.#.........#.#\n\
\#.#.#.#########.#\n\
\#S#.............#\n\
\#################"

render :: HashSet Point -> Point -> Bool -> String
render s p b
  | p `S.member` s = "O"
  | b              = " "
  | otherwise      = "#"

main :: String -> IO ()
main rawData = do
  let testInput1 = parseInput example1
      testInput2 = parseInput example2
      realInput  = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput1
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput1
  print $ part2 testInput2
  print $ part2 realInput

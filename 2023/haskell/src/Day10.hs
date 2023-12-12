module Day10 where


-- import

import AOC            hiding (Direction (..))
import AOC.Grid.Flat
import "this" Prelude

import Data.HashSet   qualified as S


-- input

type Input = Grid Char

parseInput :: String -> Input
parseInput = from2DList . lines


-- solution

data Direction = N | E | S | W
  deriving (Show, Eq)

move :: Input -> Direction -> Point -> Maybe Point
move grid dir (Point x y) =
  if inBounds grid newPoint then Just newPoint else Nothing
  where
    newPoint = case dir of
      N -> Point x (y-1)
      E -> Point (x+1) y
      S -> Point x (y+1)
      W -> Point (x-1) y

step :: Input -> (Point, Direction) -> Maybe (Point, Direction)
step grid (p, dir) = do
  newPoint <- move grid dir p
  newDir   <- case (dir, grid ! newPoint) of
    (_, 'S') -> Just dir
    (N, '|') -> Just N
    (N, '7') -> Just W
    (N, 'F') -> Just E
    (E, '-') -> Just E
    (E, '7') -> Just S
    (E, 'J') -> Just N
    (S, '|') -> Just S
    (S, 'J') -> Just W
    (S, 'L') -> Just E
    (W, '-') -> Just W
    (W, 'L') -> Just N
    (W, 'F') -> Just S
    _        -> Nothing
  pure (newPoint, newDir)

loop :: Input -> (Point, Direction) -> Maybe (Input, [Point])
loop grid (ogP, ogD) = go (ogP, ogD)
  where
    go x@(_, inD) = do
      x'@(p, _) <- step grid x
      if p /= ogP
        then do
          (newGrid, points) <- go x'
          pure (newGrid, p : points)
        else do
          let newChar = case (inD, ogD) of
                (N, N) -> '|'
                (N, W) -> '7'
                (N, E) -> 'F'
                (E, E) -> '-'
                (E, S) -> '7'
                (E, N) -> 'J'
                (S, S) -> '|'
                (S, W) -> 'J'
                (S, E) -> 'L'
                (W, W) -> '-'
                (W, N) -> 'L'
                (W, S) -> 'F'
                ds     -> error $ "unexpected direction set: " ++ show ds
          pure (updateAt ogP newChar grid, [p])

findStartingPoint :: Input -> Point
findStartingPoint grid = head do
  p <- allPoints grid
  guard $ (grid ! p) == 'S'
  pure p

part1 :: Input -> Int
part1 grid = furthestPoint $ head $ catMaybes
  [ loop grid (start, N)
  , loop grid (start, E)
  , loop grid (start, S)
  , loop grid (start, W)
  ]
  where
    start = findStartingPoint grid
    furthestPoint l = length (snd l) `div` 2

findInnerPoints :: Input -> HashSet Point -> Point -> Int -> Int
findInnerPoints grid loopSet start maxX = go 0 False start
  where
    go !result inside p
      | px p >= maxX =
        result
      | not (p `S.member` loopSet) =
        go (result + fromEnum inside) inside (rightOf p)
      | otherwise = case grid ! p of
          '|' -> go result (not inside) (rightOf p)
          'L' -> traversePipe result inside (rightOf p) 'L'
          'F' -> traversePipe result inside (rightOf p) 'F'
          c   -> error $ "unexpected character: " ++ [c]
    traversePipe result inside p startLetter =
      case grid ! p of
        '-' -> traversePipe result inside (rightOf p) startLetter
        '7' -> go result (if startLetter == 'F' then inside else not inside) (rightOf p)
        'J' -> go result (if startLetter == 'L' then inside else not inside) (rightOf p)
        c   -> error $ "unexpected character: " ++ [c]

part2 :: Input -> Int
part2 grid = sum do
  y <- [minY..maxY]
  pure $ findInnerPoints newGrid loopSet (Point minX y) maxX
  where
    start = findStartingPoint grid
    loopSet = S.fromList loopPoints
    (newGrid, loopPoints) = head $ catMaybes
      [ loop grid (start, N)
      , loop grid (start, E)
      , loop grid (start, S)
      , loop grid (start, W)
      ]
    minX = minimum (map px loopPoints)
    maxX = maximum (map px loopPoints)
    minY = minimum (map py loopPoints)
    maxY = maximum (map py loopPoints)


-- main

example :: String
example = "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJ7F7FJ-\nL---JF-JLJ.||-FJLJJ7\n|F|F-JF---7F7-L7L|7|\n|FFJF7L7F-JF7|JL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L"


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

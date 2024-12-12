module Day12 where


-- import

import AOC
import AOC.Grid.Flat
import "this" Prelude

import Data.HashSet   qualified as S


-- input

type Input = Grid Char
type Edge  = (Point, Point)

parseInput :: String -> Input
parseInput = from2DList . lines


-- solution

part1 :: Input -> Int
part1 grid = sum $ flip evalState S.empty $ sequence do
  p <- allPoints grid
  pure do
    (perimeter, area) <- visit p
    pure $ perimeter * area
  where
    visit :: Point -> State (HashSet Point) (Int, Int)
    visit p = do
      seen <- get
      if p `S.member` seen
      then pure (0, 0)
      else do
        modify $ S.insert p
        let areaCode = grid ! p
        (perimeters, areas) <- fmap unzip $ sequence do
          neighbour <- fourSurroundingPoints p
          pure do
            let neighbCode = grid !? neighbour
            if neighbCode == Just areaCode
            then visit neighbour
            else pure (1, 0)
        pure (sum perimeters, sum areas + 1)

computeEdge :: Point -> Point -> Edge
computeEdge (Point x1 y1) (Point x2 y2)
  | x1 == x2 && y2 < y1 = let ey = max y1 y2 in (Point x1 ey, Point (x1+1) ey)
  | x1 == x2 && y2 > y1 = let ey = max y1 y2 in (Point (x1+1) ey, Point x1 ey)
  | y1 == y2 && x2 < x1 = let ex = max x1 x2 in (Point ex (y1+1), Point ex y1)
  | y1 == y2 && x2 > x1 = let ex = max x1 x2 in (Point ex y1, Point ex (y1+1))
  | otherwise = error "computeEdge: misaligned points"

deduplicateEdges :: HashSet Edge -> HashSet Edge
deduplicateEdges = S.foldl' step S.empty
  where
    step edges e =
      let previousEdge = find (comesBefore e) edges
          nextEdge     = find (comesAfter  e) edges
      in case (previousEdge, nextEdge) of
        (Nothing, Nothing) -> S.insert e edges
        (Just b,  Nothing) -> S.insert (fst b, snd e) $ S.delete b edges
        (Nothing, Just  a) -> S.insert (fst e, snd a) $ S.delete a edges
        (Just b,  Just  a) -> S.insert (fst b, snd a) $ S.delete a $ S.delete b edges
    comesBefore e candidate =
      snd candidate == fst e && (py (fst candidate) == py (snd e) || px (fst candidate) == px (snd e))
    comesAfter e candidate =
      fst candidate == snd e && (py (snd candidate) == py (fst e) || px (snd candidate) == px (fst e))

part2 :: Input -> Int
part2 grid = sum $ flip evalState S.empty $ sequence do
  p <- allPoints grid
  pure do
    (edges, area) <- visit p
    pure $ S.size edges * area
  where
    visit :: Point -> State (HashSet Point) (HashSet Edge, Int)
    visit p = do
      seen <- get
      if p `S.member` seen
      then pure (S.empty, 0)
      else do
        modify $ S.insert p
        let areaCode = grid ! p
        (edges, areas) <- fmap unzip $ sequence do
          neighbour <- fourSurroundingPoints p
          pure do
            let neighbCode = grid !? neighbour
                edge = computeEdge p neighbour
            if neighbCode == Just areaCode
            then visit neighbour
            else pure (S.singleton edge, 0)
        pure (deduplicateEdges $ S.unions edges, sum areas + 1)


-- main

example :: String
example = "\
\RRRRIICCFF\n\
\RRRRIICCCF\n\
\VVRRRCCFFF\n\
\VVRCCCJFFF\n\
\VVVVCJJCFE\n\
\VVIVCCJJEE\n\
\VVIIICJJEE\n\
\MIIIIIJJEE\n\
\MIIISIJEEE\n\
\MMMISSJEEE"

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

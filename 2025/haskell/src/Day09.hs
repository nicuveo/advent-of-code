module Day09 where

import "this" Prelude hiding (intersect)

import AOC

import Data.HashSet   qualified as S
import Data.List      qualified as L


type Input = [Point]

parseInput :: String -> Input
parseInput = parseLinesWith do
  x <- number
  symbol ","
  y <- number
  pure $ Point x y


part1 :: Input -> Int
part1 allPoints = maximum do
  (point1:otherPoints) <- L.tails allPoints
  point2 <- otherPoints
  let
    dx = abs (px point2 - px point1) + 1
    dy = abs (py point2 - py point1) + 1
    area = dx * dy
  pure area

intersect
  :: HashSet (Point, Point)
  -> (Point, Point)
  -> (Point, Point)
  -> Bool
intersect safeDirs edge1 edge2
  | horizontal edge1 && vertical edge2 = check edge1 edge2
  | horizontal edge2 && vertical edge1 = check edge2 edge1
  | otherwise = False
  where
    horizontal (p1, p2) = py p1 == py p2
    vertical   (p1, p2) = px p1 == px p2
    check (h1, h2) (v1, v2)
      | px v1 < min (px h1) (px h2) = False
      | px v1 > max (px h1) (px h2) = False
      | py h1 < min (py v1) (py v2) = False
      | py h1 > max (py v1) (py v2) = False
      | strictlyWithin (px v1) (px h1) (px h2) &&
        strictlyWithin (py h1) (py v1) (py v2) = True
      | strictlyWithin (px v1) (px h1) (px h2) =
        if py v1 == py h1
        then checkInBounds v1 W E
        else checkInBounds v1 W E
      | strictlyWithin (py h1) (py v1) (py v2) =
        if px h1 == px v1
        then checkInBounds h1 N S
        else checkInBounds h2 N S
      | otherwise = False
    strictlyWithin x v1 v2 =
      x > min v1 v2 && x < max v1 v2
    checkInBounds p (directionVector -> e1) (directionVector -> e2) =
      S.member (p, e1) safeDirs /= S.member (p, e2) safeDirs

data Orientation = CW | CCW
  deriving (Show, Eq)

orientationSign :: Orientation -> Int
orientationSign CW  =  1
orientationSign CCW = -1

computeAngleOrientationSign
  :: Point
  -> Point
  -> Point
  -> Int
computeAngleOrientationSign p1 p2 p3 =
  let dx1 = px p2 - px p1
      dy1 = py p2 - py p1
      dx2 = px p3 - px p2
      dy2 = py p3 - py p2
  in if | dx1 > 0   ->  signum dy2
        | dy1 > 0   -> -signum dx2
        | dx1 < 0   -> -signum dy2
        | dy1 < 0   ->  signum dx2
        | otherwise -> error "incorrect input"

computeShapeOrientation :: [Point] -> Orientation
computeShapeOrientation allPoints = validate $ sum do
  (p1:p2:p3:_) <- L.tails $ allPoints ++ take 2 allPoints
  pure $ computeAngleOrientationSign p1 p2 p3
  where
    validate :: Int -> Orientation
    validate = \case
      4  -> CW
      -4 -> CCW
      x  -> error $ "got incorrect value: " ++ show x

computePointInfo
  :: [Point]
  -> Orientation
  -> HashSet (Point, Point)
computePointInfo allPoints shapeOrientation = S.fromList do
  (p1:p2:p3:_) <- L.tails $ allPoints ++ take 2 allPoints
  let unitVector = signum $ p2 - p1
      os = computeAngleOrientationSign p1 p2 p3
  if | os /= orientationSign shapeOrientation ->
       (p2,) . directionVector <$> [N, S, E, W]
     | shapeOrientation == CW ->
       (p2,) <$> [turn90R unitVector, -unitVector]
     | otherwise ->
       (p2,) <$> [turn90L unitVector, -unitVector]

part2 :: Input -> (Int, (Point, Point))
part2 allPoints = maximumOn fst do
  (p1:otherPoints) <- L.tails allPoints
  p2 <- otherPoints
  let
    p3 = Point (px p2) (py p1)
    p4 = Point (px p1) (py p2)
    edges =
      [ (p1, p3)
      , (p3, p2)
      , (p2, p4)
      , (p4, p1)
      ]
  guard $ (p1, signum (p3 - p1)) `S.member` safeDirections
  guard $ (p1, signum (p4 - p1)) `S.member` safeDirections
  guard $ (p2, signum (p3 - p2)) `S.member` safeDirections
  guard $ (p2, signum (p4 - p2)) `S.member` safeDirections
  guard $ and do
    rectangleEdge <- edges
    outlineEdge   <- outline
    pure $ not $ intersect safeDirections rectangleEdge outlineEdge
  let
    dx = abs (px p2 - px p1) + 1
    dy = abs (py p2 - py p1) + 1
    area = dx * dy
  pure (area, (p1, p2))
  where
    shapeOrientation = computeShapeOrientation allPoints
    safeDirections = computePointInfo allPoints shapeOrientation
    outline = do
      (p1:p2:_) <- L.tails $ allPoints ++ take 1 allPoints
      pure (p1, p2)

example :: String
example = "\
  \7,1\n\
  \11,1\n\
  \11,7\n\
  \9,7\n\
  \9,5\n\
  \2,5\n\
  \2,3\n\
  \7,3"


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

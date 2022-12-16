module Main where


-- import

import Control.Monad
import Data.HashSet     qualified as HS
import Data.IntSet      (IntSet)
import Data.IntSet      qualified as S
import Data.List        (nub)
import Text.Parsec
import Text.Parsec.Char

import AOC


-- input

data Sensor = Sensor
  { sensorPos :: Point
  , beaconPos :: Point
  }
  deriving (Eq, Show)

type Input = [Sensor]

parseInput :: String -> Input
parseInput = parseLinesWith do
  symbol "Sensor at x="
  sensorX <- number
  symbol ", y="
  sensorY <- number
  symbol ": closest beacon is at x="
  beaconX <- number
  symbol ", y="
  beaconY <- number
  pure $ Sensor (Point sensorX sensorY) (Point beaconX beaconY)


-- part 1: counting ints

part1 :: Int -> Input -> Int
part1 target = S.size . S.unions . map \Sensor {..} ->
  let distance = manhattanNorm $ beaconPos - sensorPos
      vertical = abs (target - py sensorPos)
      xRange   = distance - vertical
      xCenter  = px sensorPos
      result   = S.fromList [xCenter-xRange..xCenter+xRange]
  in if py beaconPos == target
     then S.delete (px beaconPos) result
     else result


-- part 2: alternate coordinate system

-- | converts a point from our original coordinate system
-- into an alternate diagonal coordinate system
--
-- the X "unit vector" of our new system is (0.5, -0.5)
-- the Y "unit vector" of our new system is (0.5,  0.5)
toSkewed :: Point -> Point
toSkewed (Point x y) = Point (x-y) (x+y)

-- | converts back to our old coordinate system
toRealPoint :: Point -> Point
toRealPoint (Point x y) = Point
  ((y + x) `div` 2)
  ((y - x) `div` 2)

-- | given a sensor, output the coordinate of its AABB
-- (axis-aligned bounding box):
--   (xMin, xMax), (yMin, yMax)
-- those coordinates are *inclusive*
skewedRanges :: Sensor -> ((Int, Int), (Int, Int))
skewedRanges Sensor {..} =
  ( (minX, maxX)
  , (minY, maxY)
  )
  where
    distance = manhattanNorm $ beaconPos - sensorPos
    corners  = [ sensorPos + Point   distance  0
               , sensorPos + Point (-distance) 0
               , sensorPos + Point 0   distance
               , sensorPos + Point 0 (-distance)
               ]
    skewedCorners = map toSkewed corners
    minX = minimum $ map px skewedCorners
    maxX = maximum $ map px skewedCorners
    minY = minimum $ map py skewedCorners
    maxY = maximum $ map py skewedCorners

-- | checks whether the given point is within the bounds
-- of the given box (inclusive!)
inBox :: Point -> ((Int, Int),(Int, Int)) -> Bool
inBox (Point x y) ((xMin, xMax), (yMin, yMax)) =
  and [ x >= xMin
      , x <= xMax
      , y >= yMin
      , y <= yMax
      ]

part2 :: Int -> Input -> Int
part2 threshold sensors = computeResult $ head do
  let
    sensorBoxes = map skewedRanges sensors
    (xRanges, yRanges) = unzip sensorBoxes
  x <- getCandidateValues xRanges
  y <- getCandidateValues yRanges
  let skewedPoint = Point x y
      realPoint = toRealPoint skewedPoint
  guard $ not $ any (inBox skewedPoint) sensorBoxes
  guard $ realPoint `inBox` ((0, threshold), (0, threshold))
  pure realPoint
  where
    computeResult (Point x y) = x * 4000000 + y
    -- given that there is only one valid point, it MUST
    -- be surrounded by invalid point, therefore it must
    -- lie just beyond the border of at least one sensor
    getCandidateValues rs = nub do
      (start, end) <- rs
      [start-1, end+1]


-- main

main :: IO ()
main = aocMain 15 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1      10 testInput
  print $ part1 2000000 realInput
  putStrLn "# Part 2"
  print $ part2      20 testInput
  print $ part2 4000000 realInput

example :: String
example = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3"

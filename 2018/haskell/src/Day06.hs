-- module

module Day06 (day06_1, day06_2) where



-- import

import           Data.List
import           Data.List.Split
import           Safe
import           Text.Printf

import           Common



-- solution

day06_1 :: Solution
day06_1 input = show $ snd $ head validPoints
  where allPoints = parseInput input
        xMin = minimum $ snd <$> allPoints
        xMax = maximum $ snd <$> allPoints
        yMin = minimum $ fst <$> allPoints
        yMax = maximum $ fst <$> allPoints
        attributedPoints =
          [ (findPointClosestTo (y,x) allPoints, (y,x))
          | y <- [yMin .. yMax]
          , x <- [xMin .. xMax]
          ]
        groupedPoints = sortOn (negate . length) $ groupOn fst $ sort attributedPoints
        validPoints = [ ((y,x), length ps)
                      | ps@((Just (y,x), _):_) <- groupedPoints
                      , not $ any onEdge $ snd <$> ps
                      ]
        onEdge (y, x) = y == yMin || y == yMax || x == xMin || x == xMax


day06_2 :: Solution
day06_2 input = show regionSize
  where allPoints = parseInput input
        xMin = minimum $ snd <$> allPoints
        xMax = maximum $ snd <$> allPoints
        yMin = minimum $ fst <$> allPoints
        yMax = maximum $ fst <$> allPoints
        regionSize = countTrue
          [ sum (distanceFrom (y,x) <$> allPoints) < 10000
          | y <- [yMin .. yMax]
          , x <- [xMin .. xMax]
          ]



-- helpers

display :: Int -> Int -> [Point] -> [(Maybe Point, Point)] -> String
display xMin xMax allPoints ps =
  unlines [ unwords $ toString . fst <$> line
          | line <- chunksOf (xMax + 1 - xMin) ps
          ]
  where toString Nothing      = ".."
        toString (Just (y,x)) = printf "%02d" $ elemIndexJust (y,x) allPoints

distanceFrom :: Point -> Point -> Int
distanceFrom (y1, x1) (y2, x2) = abs (y2 - y1) + abs (x2 - x1)

findPointClosestTo :: Point -> [Point] -> Maybe Point
findPointClosestTo p points = if d1 == d2 then Nothing else Just p1
  where ((d1, p1):(d2, _):_) = sort [(distanceFrom p q, q) | q <- points]


type Point = (Int, Int)

parseInput :: String -> [Point]
parseInput = map (parseWith coordinates) . lines
  where coordinates = do
          y <- intParser
          symbol ","
          x <- intParser
          return (y, x)

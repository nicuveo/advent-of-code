module AOC.Grid.Sparse where


-- imports

import "this" Prelude

import Data.HashMap.Strict qualified as M
import Safe

import AOC.Point


-- type

type Grid = M.HashMap Point


-- constructors

fromList :: Int -> Int -> [a] -> Grid a
fromList width height raw
  | length raw /= width * height = error "fromList: wrong number of elements"
  | otherwise = M.fromList $ zip indices raw
  where indices = [Point x y | y <- [0..height-1], x <- [0..width-1]]

from2DList :: [[a]] -> Grid a
from2DList lists
  | any ((/= width) . length) lists = error "from2DList: not all rows have the same length"
  | otherwise = fromList width height $ concat lists
  where width  = maybe 0 length $ headMay lists
        height = length lists


-- accessors

(!) :: Grid a -> Point -> a
(!) = (M.!)

(!?) :: Grid a -> Point -> Maybe a
(!?) = flip M.lookup

xRange :: Grid a -> [Int]
xRange g = let (a,b) = bounds g in [px a .. px b]

yRange :: Grid a -> [Int]
yRange g = let (a,b) = bounds g in [py a .. py b]

allPoints :: Grid a -> [Point]
allPoints g = Point <$> xRange g <*> yRange g


-- boundary checking

bounds :: Grid a -> (Point, Point)
bounds m = ( Point (minimum xs) (minimum ys)
           , Point (maximum xs) (maximum ys)
           )
  where ys = py <$> ks
        xs = px <$> ks
        ks = M.keys m

inBounds :: Grid a -> Point -> Bool
inBounds g (Point y x) = and [ y >= minY , y <= maxY
                             , x >= minX , x <= maxX
                             ]
  where (Point minY minX, Point maxY maxX) = bounds g

gridFourSurroundingPoints :: Grid a -> Point -> [Point]
gridFourSurroundingPoints f = filter (inBounds f) . fourSurroundingPoints

gridEightSurroundingPoints :: Grid a -> Point -> [Point]
gridEightSurroundingPoints f = filter (inBounds f) . eightSurroundingPoints

gridFourNeighbours :: Grid a -> Point -> [a]
gridFourNeighbours f = map (f !) . gridFourSurroundingPoints f

gridEightNeighbours :: Grid a -> Point -> [a]
gridEightNeighbours f = map (f !) . gridEightSurroundingPoints f


-- display

displayWith :: (Point -> Maybe a -> String) -> Grid a -> String
displayWith f g =
  unlines [ concat [ let p = Point x y in f p $ g !? p
                   | x <- xRange g
                   ]
          | y <- yRange g
          ]

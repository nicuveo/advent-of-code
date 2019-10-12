module AOC.Map.Sparse where



-- imports

import qualified Data.Map.Strict as M
import           Data.Maybe
import           Safe

import           AOC.Point



-- type

type SparseMap = M.Map Point



-- constructors

fromList :: Int -> Int -> [a] -> SparseMap a
fromList width height raw
  | length raw /= width * height = error "fromList: wrong number of elements"
  | otherwise = M.fromList $ zip indices raw
  where indices = [Point y x | y <- [0..height-1], x <- [0..width-1]]

from2DList :: [[a]] -> SparseMap a
from2DList lists
  | any ((/= width) . length) lists = error "from2DList: not all rows have the same length"
  | otherwise = fromList width height $ concat lists
  where width  = maybe 0 length $ headMay lists
        height = length lists



-- accessors

(!) :: SparseMap a -> Point -> a
(!) = (M.!)

(!?) :: SparseMap a -> Point -> Maybe a
(!?) = (M.!?)

mapYs :: SparseMap a -> [Int]
mapYs m = let (a,b) = bounds m in [py a .. py b]

mapXs :: SparseMap a -> [Int]
mapXs m = let (a,b) = bounds m in [px a .. px b]

allPoints :: SparseMap a -> [Point]
allPoints m = Point <$> mapYs m <*> mapXs m



-- boundary checking

bounds :: SparseMap a -> (Point, Point)
bounds m = ( Point (minimum ys) (minimum xs)
           , Point (maximum ys) (maximum xs)
           )
  where ys = py <$> ks
        xs = px <$> ks
        ks = M.keys m

inBounds :: SparseMap a -> Point -> Bool
inBounds g (Point y x) = and [ y >= minY , y <= maxY
                             , x >= minX , x <= maxX
                             ]
  where (Point minY minX, Point maxY maxX) = bounds g

fourNeighbouringPointsOf :: SparseMap a -> Point -> [Point]
fourNeighbouringPointsOf f = filter (inBounds f) . fourNeighboursOf

eightNeighbouringPointsOf :: SparseMap a -> Point -> [Point]
eightNeighbouringPointsOf f = filter (inBounds f) . eightNeighboursOf

fourMapNeighboursOf :: SparseMap a -> Point -> [a]
fourMapNeighboursOf f = mapMaybe (f !?) . fourNeighbouringPointsOf f

eightMapNeighboursOf :: SparseMap a -> Point -> [a]
eightMapNeighboursOf f = mapMaybe (f !?) . eightNeighbouringPointsOf f



-- display

displayWith :: (Point -> a -> String) -> String -> SparseMap a -> String
displayWith g e m =
  unlines [ concat [ let p = Point y x in maybe e (g p) $ m !? p
                   | x <- mapXs m
                   ]
          | y <- mapYs m
          ]

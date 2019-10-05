module AOC.Map.Sparse where



-- imports

import qualified Data.Map.Strict as M
import           Safe

import           AOC.Point



-- type

type SparseMap = M.Map Point



-- constructors

makeSparseMapFromList :: Int -> Int -> [a] -> SparseMap a
makeSparseMapFromList width height raw
  | length raw /= width * height = error "makeSparseMapFromList: wrong number of elements"
  | otherwise = M.fromList $ zip indices raw
  where indices = [Point y x | y <- [0..height-1], x <- [0..width-1]]

makeSparseMapFrom2DList :: [[a]] -> SparseMap a
makeSparseMapFrom2DList lists
  | any ((/= width) . length) lists = error "makeSparseMapFrom2DList: not all rows have the same length"
  | otherwise = makeSparseMapFromList width height $ concat lists
  where width  = maybe 0 length $ headMay lists
        height = length lists



-- accessors

(!) :: SparseMap a -> Point -> a
(!) = (M.!)

(!?) :: SparseMap a -> Point -> Maybe a
(!?) = (M.!?)



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

fourMapNeighboursOf :: SparseMap a -> Point -> [Point]
fourMapNeighboursOf f = filter (inBounds f) . fourNeighboursOf

eightMapNeighboursOf :: SparseMap a -> Point -> [Point]
eightMapNeighboursOf f = filter (inBounds f) . eightNeighboursOf

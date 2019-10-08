module AOC.Map.Flat where



-- imports

import           Control.Exception
import qualified Data.Vector.Unboxed as V
import           Safe

import           AOC.Point



-- type

data FlatMap a = FlatMap { fmapData   :: V.Vector a
                         , fmapWidth  :: {-# UNPACK #-} !Int
                         , fmapHeight :: {-# UNPACK #-} !Int
                         }

type Index = Int



-- constructors

fromList :: V.Unbox a => Int -> Int -> [a] -> FlatMap a
fromList width height raw
  | length raw /= width * height = error "makeFlatMapFromList: wrong number of elements"
  | otherwise = FlatMap (V.fromList raw) width height

from2DList :: V.Unbox a => [[a]] -> FlatMap a
from2DList lists
  | any ((/= width) . length) lists = error "makeFlatMapFrom2DList: not all rows have the same length"
  | otherwise = FlatMap (V.fromList $ concat lists) width height
  where width  = maybe 0 length $ headMay lists
        height = length lists



-- accessors

(!) :: V.Unbox a => FlatMap a -> Point -> a
g ! p =  fmapData g V.! toIndex g p

(!?) :: V.Unbox a => FlatMap a -> Point -> Maybe a
g !? p
  | inBounds g p = Just $ g ! p
  | otherwise    = Nothing

mapYs :: FlatMap a -> [Int]
mapYs m = [0 .. fmapHeight m - 1]

mapXs :: FlatMap a -> [Int]
mapXs m = [0 .. fmapWidth m - 1]



-- update

updateWith :: (V.Unbox a, V.Unbox b) => (V.Vector a -> V.Vector b) -> FlatMap a -> FlatMap b
updateWith f m@(FlatMap v _ _) = m { fmapData = assert (V.length v == V.length w) w }
  where w = f v



-- mapping

pmap :: (V.Unbox a, V.Unbox b) => (Point -> a -> b) -> FlatMap a -> FlatMap b
pmap f g = updateWith (V.imap $ f . fromIndex g) g



-- boundary checking

bounds :: FlatMap a -> (Point, Point)
bounds m = ( Point 0 0
           , Point (fmapHeight m -1) (fmapWidth m - 1)
           )

inBounds :: FlatMap a -> Point -> Bool
inBounds m (Point y x) = and [ y >= 0, y < fmapHeight m
                             , x >= 0, x < fmapWidth  m
                             ]

fourNeighbouringPointsOf :: FlatMap a -> Point -> [Point]
fourNeighbouringPointsOf f = filter (inBounds f) . fourNeighboursOf

eightNeighbouringPointsOf :: FlatMap a -> Point -> [Point]
eightNeighbouringPointsOf f = filter (inBounds f) . eightNeighboursOf

fourMapNeighboursOf :: V.Unbox a => FlatMap a -> Point -> [a]
fourMapNeighboursOf f = map (f !) . fourNeighbouringPointsOf f

eightMapNeighboursOf :: V.Unbox a => FlatMap a -> Point -> [a]
eightMapNeighboursOf f = map (f !) . eightNeighbouringPointsOf f



-- conversion

toIndex :: FlatMap a -> Point -> Index
toIndex m (Point y x) = y * fmapWidth m + x

fromIndex :: FlatMap a -> Index -> Point
fromIndex m i = uncurry Point $ divMod i $ fmapWidth m



-- display

displayWith :: V.Unbox a => (Point -> a -> String) -> FlatMap a -> String
displayWith f g =
  unlines [ concat [ let p = Point y x in f p $ g ! p
                   | x <- mapXs g
                   ]
          | y <- mapYs g
          ]

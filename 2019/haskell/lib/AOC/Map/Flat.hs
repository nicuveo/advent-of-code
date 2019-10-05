module AOC.Map.Flat where



-- imports

import           Control.Exception
import           Data.Function       (on)
import           Data.Maybe
import qualified Data.Vector.Unboxed as V
import           Safe

import           AOC.Misc
import           AOC.Point



-- type

data FlatMap a = FlatMap { fmapData   :: V.Vector a
                         , fmapWidth  :: {-# UNPACK #-} !Int
                         , fmapHeight :: {-# UNPACK #-} !Int
                         }

type Index = Int



-- constructors

makeFlatMapFromList :: V.Unbox a => Int -> Int -> [a] -> FlatMap a
makeFlatMapFromList width height raw
  | length raw /= width * height = error "makeFlatMapFromList: wrong number of elements"
  | otherwise = FlatMap (V.fromList raw) width height

makeFlatMapFrom2DList :: V.Unbox a => [[a]] -> FlatMap a
makeFlatMapFrom2DList lists
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



-- update

updateMap :: V.Unbox a => (V.Vector a -> V.Vector a) -> FlatMap a -> FlatMap a
updateMap f m@(FlatMap v _ _) = m { fmapData = assert (v `sameLength` w) w }
  where w = f v
        sameLength = (==) `on` V.length



-- boundary checking

bounds :: FlatMap a -> (Point, Point)
bounds m = ( Point 0 0
           , Point (fmapHeight m -1) (fmapWidth m - 1)
           )

inBounds :: FlatMap a -> Point -> Bool
inBounds m (Point y x) = and [ y >= 0, y < fmapHeight m
                             , x >= 0, x < fmapWidth  m
                             ]

fourMapNeighboursOf :: FlatMap a -> Point -> [Point]
fourMapNeighboursOf f = filter (inBounds f) . fourNeighboursOf

eightMapNeighboursOf :: FlatMap a -> Point -> [Point]
eightMapNeighboursOf f = filter (inBounds f) . eightNeighboursOf



-- conversion

toIndex :: FlatMap a -> Point -> Index
toIndex m (Point y x) = y * fmapWidth m + x

fromIndex :: FlatMap a -> Index -> Point
fromIndex m i = uncurry Point $ divMod i $ fmapWidth m

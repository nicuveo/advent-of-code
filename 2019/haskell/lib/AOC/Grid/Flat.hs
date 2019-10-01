module AOC.Grid.Flat ( Grid (gridData, gridWidth, gridHeight)
                     , makeGridFromList, makeGridFrom2DList
                     , bounds, inBounds
                     ) where



-- imports

import qualified Data.Vector.Unboxed as V
import           Safe

import           AOC.Point



-- type

data Grid a = Grid { gridData   :: V.Vector a
                   , gridWidth  :: {-# UNPACK #-} !Int
                   , gridHeight :: {-# UNPACK #-} !Int
                   }



-- exported constructors

makeGridFromList :: V.Unbox a => Int -> Int -> [a] -> Grid a
makeGridFromList width height raw
  | length raw /= width * height = error "makeGridFromList: wrong number of elements"
  | otherwise = Grid (V.fromList raw) width height

makeGridFrom2DList :: V.Unbox a => [[a]] -> Grid a
makeGridFrom2DList lists
  | any ((/= width) . length) lists = error "makeGridFrom2DList: not all rows have the same length"
  | otherwise = Grid (V.fromList $ concat lists) width height
  where width  = maybe 0 length $ headMay lists
        height = length lists



-- useful functions

bounds :: Grid a -> (Point, Point)
bounds g = ( Point 0 0
           , Point (gridHeight g -1) (gridWidth g - 1)
           )

inBounds :: Grid a -> Point -> Bool
inBounds g (Point y x) = and [ y >= 0, y < gridHeight g
                             , x >= 0, x < gridWidth  g
                             ]

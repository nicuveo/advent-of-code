module AOC.Grid.Sparse where



-- imports

import qualified Data.Map.Strict as M

import           AOC.Point



-- type

type Grid a = M.Map Point a



-- useful functions

bounds :: Grid a -> (Point, Point)
bounds g = ( Point (minimum ys) (minimum xs)
           , Point (maximum ys) (maximum xs)
           )
  where ys = py <$> ks
        xs = px <$> ks
        ks = M.keys g

inBounds :: Grid a -> Point -> Bool
inBounds g (Point y x) = and [ y >= minY , y <= maxY
                             , x >= minX , x <= maxX
                             ]
  where (Point minY minX, Point maxY maxX) = bounds g

module AOC.Grid.Flat where


-- imports

import "this" Prelude

import Control.Exception
import Data.Vector       qualified as V
import Safe

import AOC.Point


-- type

data Grid a = Grid
  { gData   :: V.Vector a
  , gWidth  :: Int
  , gHeight :: Int
  }
  deriving (Eq, Functor, Ord)

type Index = Int


-- constructors

fromList :: Int -> Int -> [a] -> Grid a
fromList width height raw
  | length raw /= width * height = error "fromList: wrong number of elements"
  | otherwise = Grid (V.fromList raw) width height

from2DList :: [[a]] -> Grid a
from2DList lists
  | any ((/= width) . length) lists = error "from2DList: not all rows have the same length"
  | otherwise = Grid (V.fromList $ concat lists) width height
  where width  = maybe 0 length $ headMay lists
        height = length lists


-- to list

toList :: Grid a -> [a]
toList = V.toList . gData


-- accessors

(!) :: Grid a -> Point -> a
g ! p =  gData g V.! toIndex g p

(!?) :: Grid a -> Point -> Maybe a
g !? p
  | inBounds g p = Just $ g ! p
  | otherwise    = Nothing

xRange :: Grid a -> [Int]
xRange g = [0 .. gWidth g - 1]

yRange :: Grid a -> [Int]
yRange g = [0 .. gHeight g - 1]

allPoints :: Grid a -> [Point]
allPoints g = Point <$> xRange g <*> yRange g


-- update

updateAt :: Point -> a -> Grid a -> Grid a
updateAt p x g@(Grid v w h) = Grid (v V.// [(toIndex g p, x)]) w h

updateWith :: (V.Vector a -> V.Vector b) -> Grid a -> Grid b
updateWith f m@(Grid v _ _) = m { gData = assert (V.length v == V.length w) w }
  where w = f v


-- mapping

pmap :: (Point -> a -> b) -> Grid a -> Grid b
pmap f g = updateWith (V.imap $ f . fromIndex g) g


-- boundary checking

bounds :: Grid a -> (Point, Point)
bounds m = ( Point 0 0
           , Point (gWidth m - 1) (gHeight m -1)
           )

inBounds :: Grid a -> Point -> Bool
inBounds m (Point x y) = and [ y >= 0, y < gHeight m
                             , x >= 0, x < gWidth  m
                             ]

gridFourSurroundingPoints :: Grid a -> Point -> [Point]
gridFourSurroundingPoints f = filter (inBounds f) . fourSurroundingPoints

gridEightSurroundingPoints :: Grid a -> Point -> [Point]
gridEightSurroundingPoints f = filter (inBounds f) . eightSurroundingPoints

gridFourNeighbours :: Grid a -> Point -> [a]
gridFourNeighbours f = map (f !) . gridFourSurroundingPoints f

gridEightNeighbours :: Grid a -> Point -> [a]
gridEightNeighbours f = map (f !) . gridEightSurroundingPoints f


-- conversion

toIndex :: Grid a -> Point -> Index
toIndex m (Point x y) = y * gWidth m + x

fromIndex :: Grid a -> Index -> Point
fromIndex g i = let (y,x) = divMod i $ gWidth g in Point x y


-- display

displayWith :: (Point -> a -> String) -> Grid a -> String
displayWith f g =
  unlines [ concat [ let p = Point x y in f p $ g ! p
                   | x <- xRange g
                   ]
          | y <- yRange g
          ]

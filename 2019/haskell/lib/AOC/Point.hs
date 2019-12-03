module AOC.Point where



-- imports

import           Text.Printf



-- points

data Point = Point { py :: {-# UNPACK #-} !Int
                   , px :: {-# UNPACK #-} !Int
                   } deriving (Eq, Ord)

instance Show Point where
  show (Point y x) = printf "(y: %d, x: %2d)" y x

type Vector = Point



-- num instance
-- note: this is Wrong™, but very convenient

instance Num Point where
  (Point y1 x1) + (Point y2 x2) = Point (y1 + y2) (x1 + x2)
  (Point y1 x1) - (Point y2 x2) = Point (y1 - y2) (x1 - x2)
  abs    (Point y x) = Point (abs    y) (abs    x)
  negate (Point y x) = Point (negate y) (negate x)
  signum (Point y x) = Point (signum y) (signum x)
  fromInteger _ = error "can't implictly convert int to vector"
  (*) = error "tried to multiply two 2D vectors"



-- cardinal coordinates

data Direction = N | NE | E | SE | S | SW | W | NW
  deriving (Show, Eq, Bounded, Enum)

directionVector :: Direction -> Vector
directionVector cd = case cd of
                      N  -> Point u 0
                      NE -> Point u r
                      E  -> Point 0 r
                      SE -> Point d r
                      S  -> Point d 0
                      SW -> Point d l
                      W  -> Point 0 l
                      NW -> Point u l
  where u = -1
        d =  1
        l = -1
        r =  1



-- rotation

rotate90R :: Vector -> Vector
rotate90R (Point y x) = Point x (-y)

rotate90L :: Vector -> Vector
rotate90L (Point y x) = Point (-x) y



-- other operators

(.*) :: Int -> Vector -> Vector
k .* (Point y x) = Point (k * y) (k * x)

(*.) :: Vector -> Int -> Vector
(*.) = flip (.*)



-- useful functions

sqNorm :: Vector -> Int
sqNorm (Point y x) = y * y + x * x

above, below, leftOf, rightOf :: Point -> Point
above   (Point y x) = Point (y-1) x
below   (Point y x) = Point (y+1) x
leftOf  (Point y x) = Point y (x-1)
rightOf (Point y x) = Point y (x+1)

fourNeighboursOf :: Point -> [Point]
fourNeighboursOf p = [above p, rightOf p, below p, leftOf p]

eightNeighboursOf :: Point -> [Point]
eightNeighboursOf (Point y x) =
  [ Point (y-1) (x-1), Point (y-1) x, Point (y-1) (x+1)
  , Point  y    (x-1)               , Point  y    (x+1)
  , Point (y+1) (x-1), Point (y+1) x, Point (y+1) (x+1)
  ]

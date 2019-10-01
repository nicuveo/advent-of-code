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
  fromInteger x = Point 0 $ fromInteger x
  (*) = error "tried to multiply two 2D vectors"



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

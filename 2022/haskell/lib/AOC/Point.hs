module AOC.Point where


-- imports

import Data.Hashable
import GHC.Generics  (Generic)
import Text.Printf


-- points

data Point = Point
  { px :: Int
  , py :: Int
  }
  deriving (Eq, Generic, Ord)

instance Show Point where
  show (Point x y) = printf "(x: %d, y: %d)" x y

instance Hashable Point where

type Vector = Point


-- num instance
-- note: this is Wrong™, but very convenient

instance Num Point where
  (Point x1 y1) + (Point x2 y2) = Point (x1 + x2) (y1 + y2)
  (Point x1 y1) - (Point x2 y2) = Point (x1 - x2) (y1 - y2)
  abs    (Point x y) = Point (abs    x) (abs    y)
  negate (Point x y) = Point (negate x) (negate y)
  signum (Point x y) = Point (signum x) (signum y)
  fromInteger x = Point (fromInteger x) 0
  (*) = error "tried to multiply two 2D vectors"


-- cardinal coordinates

data Direction
  = N
  | NE
  | E
  | SE
  | S
  | SW
  | W
  | NW
  deriving (Bounded, Enum, Eq, Show)

instance Hashable Direction where
  hashWithSalt x = hashWithSalt x . fromEnum

directionVector :: Direction -> Vector
directionVector = \case
  N  -> Point 0 u
  NE -> Point r u
  E  -> Point r 0
  SE -> Point r d
  S  -> Point 0 d
  SW -> Point l d
  W  -> Point l 0
  NW -> Point l u
  where
    u = -1
    d =  1
    l = -1
    r =  1


-- rotation

class Rotatable a where
  turn90L :: a -> a
  turn90R :: a -> a

instance Rotatable Direction where
  turn90L = \case
    N  -> W
    NE -> NW
    E  -> N
    SE -> NE
    S  -> E
    SW -> SE
    W  -> S
    NW -> SW
  turn90R = \case
    N  -> E
    NE -> SE
    E  -> S
    SE -> SW
    S  -> W
    SW -> NW
    W  -> N
    NW -> NE

instance Rotatable Vector where
  turn90L (Point x y) = Point y (-x)
  turn90R (Point x y) = Point (-y) x


-- other operators

(.*) :: Int -> Vector -> Vector
k .* (Point x y) = Point (k * x) (k * y)

(*.) :: Vector -> Int -> Vector
(*.) = flip (.*)


-- useful functions

sqaredNorm :: Vector -> Int
sqaredNorm (Point x y) = x * x + y * y

manhattanNorm :: Vector -> Int
manhattanNorm (Point x y) = abs x + abs y

origin :: Point
origin = Point 0 0

xUnit :: Vector
xUnit = Point 1 0

yUnit :: Vector
yUnit = Point 0 1

above, below, leftOf, rightOf :: Point -> Point
above   (Point x y) = Point x (y-1)
below   (Point x y) = Point x (y+1)
leftOf  (Point x y) = Point (x-1) y
rightOf (Point x y) = Point (x+1) y

fourSurroundingPoints :: Point -> [Point]
fourSurroundingPoints p = [above p, rightOf p, below p, leftOf p]

eightSurroundingPoints :: Point -> [Point]
eightSurroundingPoints (Point y x) =
  [ Point (x-1) (y-1) , Point x (y-1) , Point (x+1) (y-1)
  , Point (x-1)  y    ,                 Point (x+1)  y
  , Point (x-1) (y+1) , Point x (y+1) , Point (x+1) (y+1)
  ]

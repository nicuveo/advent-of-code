-- module

module Day22 (day22_1, day22_2) where



-- import

import           Control.Monad
import           Data.Function
import qualified Data.Map             as M
import           Data.MemoCombinators

import           Common
import           PathFinding



-- solution

day22_1 :: Solution
day22_1 s = show $ sum [fromEnum $ regionType i (y,x) | y <- [0 .. ty], x <- [0 .. tx]]
  where i@(_, (ty, tx)) = parseInput s


day22_2 :: Solution
day22_2 = show . shortestPath . parseInput

-- shortestPath :: Input -> Int
shortestPath i@(_, target@(ty,tx)) = findPathH neighbours heuristic (0,0,Torch) (ty,tx,Torch)
  where neighbours (y,x,t) = (7, (y,x,other t $ regionType i (y,x))) : do
          (ny,nx) <- [(y-1,x), (y,x+1), (y+1,x), (y,x-1)]
          let nk = regionType i (ny,nx)
          guard $ ny >= 0 && nx >= 0
          guard $ t `allowedIn` nk
          return (1, (ny,nx,t))
        heuristic (y,x,t) = abs (y - ty) + abs(x - tx) + (if t == Torch then 0 else 7)





-- helpers

type Point = (Int, Int)
type Seed  = Int
type Input = (Seed, Point)

parseInput :: String -> Input
parseInput = parseWith $ liftM2 (,) depth target
  where depth  = symbol "depth:" >> intParser
        target = do
          symbol "target:"
          x <- intParser
          symbol ","
          y <- intParser
          return (y,x)

z = (`mod` 20183)

geoLevel :: Input -> Point -> Int
geoLevel = memo2 (pair integral point) point gl
  where point = pair integral integral
        gl i@(_, target) p@(y,x)
          | target == p = 0
          | y == 0      = x * 16807
          | x == 0      = y * 48271
          | otherwise   = z $ erosionLevel i (y-1,x) * erosionLevel i (y,x-1)


erosionLevel :: Input -> Point -> Int
erosionLevel i@(seed, _) p = z $ geoLevel i p + seed

regionType :: Input -> Point -> Type
regionType = (toEnum . (`mod` 3)) ... erosionLevel


data Type = Rocky | Wet | Narrow
  deriving (Eq, Ord, Enum, Bounded)

instance Show Type where
  show Rocky  = "."
  show Wet    = "="
  show Narrow = "|"

data Tool = Neither | Torch | Gear
  deriving (Show, Eq, Ord, Enum, Bounded)

allowedIn :: Tool -> Type -> Bool
allowedIn Neither Rocky  = False
allowedIn Torch   Wet    = False
allowedIn Gear    Narrow = False
allowedIn _       _      = True

other :: Tool -> Type -> Tool
other t k = head [nt | nt <- [minBound .. maxBound], nt /= t, nt `allowedIn` k]

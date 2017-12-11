-- module

module Day11 (day11_1, day11_2) where



-- import

import           Common

import           Data.Function
import           Data.List
import           Text.Parsec



-- solution

day11_1 :: Solution
day11_1 = show . distanceFromOrigin . foldl1' addVectors . map toVector . parseSteps


day11_2 :: Solution
day11_2 = show . maximum . map distanceFromOrigin . scanl1 addVectors . fmap toVector . parseSteps



-- helpers

data Direction = N | NE | SE | S | SW | NW deriving (Show, Eq)
type Vector = (Int, Int)

toVector :: Direction -> Vector
toVector N  = ( 0, 1)
toVector NE = (-1, 1)
toVector SE = (-1, 0)
toVector S  = ( 0,-1)
toVector SW = ( 1,-1)
toVector NW = ( 1, 0)

distanceFromOrigin :: Vector -> Int
distanceFromOrigin (x, y) = if   on (==) signum x y
                            then on (+)  abs    x y
                            else on max  abs    x y

addVectors :: Vector -> Vector -> Vector
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

parseSteps :: String -> [Direction]
parseSteps = parseWith steps
  where steps = dir `sepBy` char ','
        dir   = tryAll [nw, sw, ne, se, n, s]
        nw    = string "nw" >> return NW
        sw    = string "sw" >> return SW
        ne    = string "ne" >> return NE
        se    = string "se" >> return SE
        n     = string "n"  >> return N
        s     = string "s"  >> return S

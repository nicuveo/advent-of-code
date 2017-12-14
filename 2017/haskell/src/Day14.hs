-- module

module Day14 (day14_1, day14_2) where



-- import

import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.Graph
import qualified Data.Vector.Unboxed as V
import           Text.Printf

import           Common
import           Day10



-- solution

day14_1 :: Solution
day14_1 = show . sum . map (popCount . digitToInt) . (knotHash <=< keys)


day14_2 :: Solution
day14_2 = show . length . components . toGraph . toIntVec . (knotHash <=< keys)



-- helpers

type IntVec = V.Vector Int
type Pos    = (Int, Int)

gSize :: Int
gSize = 128

knotHash :: String -> String
knotHash = day10_2

keys :: String -> [String]
keys root = [ printf "%s-%d" root line
            | line <- [0..gSize-1] :: [Int]
            ]

toIntVec :: String -> IntVec
toIntVec = V.fromList . map digitToInt

isUsed :: IntVec -> Pos -> Bool
isUsed g (x, y)
  | x < 0 || y < 0 || x >= gSize || y >= gSize = False
  | otherwise = testBit (g V.! (y * div gSize 4 + div x 4)) $ 3 - mod x 4

toGraph :: IntVec -> Graph
toGraph g = graph
  where (graph, _, _) = graphFromEdges $ do
          p1y <- [0..gSize-1]
          p1x <- [0..gSize-1]
          let p1 = (p1x, p1y)
          guard $ isUsed g p1
          return (p1, p1, p1 : [ p2
                               | p2 <- [(p1x, p1y-1), (p1x-1, p1y)]
                               , isUsed g p2
                               ])

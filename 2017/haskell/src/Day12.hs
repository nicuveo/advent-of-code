-- module

module Day12 (day12_1, day12_2) where



-- import

import           Control.Monad
import           Data.List
import           Data.Map.Strict as M hiding (foldl')
import           Data.Maybe
import           Text.Parsec

import           Common



-- solution

day12_1 :: Solution
day12_1 = show . length . reachable 0 . fromEdges . (parseEdges <=< lines)


day12_2 :: Solution
day12_2 = show . length . groups . fromEdges . (parseEdges <=< lines)



-- helpers

type Vertex = Int
type Edge   = (Int, Int)
type Graph  = Map Vertex [Vertex]

parseEdges :: String -> [Edge]
parseEdges = parseWith line
  where line = do
          x <- intParser
          string " <-> "
          ys <- intParser `sepBy` string ", "
          return $ concat [[(x,y),(y,x)] | y <- ys]

addEdge :: Edge -> Graph -> Graph
addEdge (x, y) = insertWith (++) x [y]

fromEdges :: [Edge] -> Graph
fromEdges = M.map (nub . sort) . foldl' (flip addEdge) empty

reachable :: Vertex -> Graph -> [Vertex]
reachable v g = walk [] v
  where walk seen new
          | new `elem` seen = seen
          | otherwise       = foldl' walk (new:seen) $ fromMaybe [] $ M.lookup new g

groups :: Graph -> [[Vertex]]
groups g = foldl' getGroup [] $ keys g
  where getGroup gs v
          | any (v `elem`) gs = gs
          | otherwise         = reachable v g : gs

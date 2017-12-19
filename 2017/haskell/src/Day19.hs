-- module

module Day19 (day19_1, day19_2) where



-- import

import           Data.Char
import qualified Data.Graph      as G
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe

import           Common



-- solution

day19_1 :: Solution
day19_1 input = filter isAlphaNum $ map getNodeChar $ G.reachable graph root
  where (graph, root, getNodeChar, _) = mkGraph input


day19_2 :: Solution
day19_2 input = show $ 1 + sum [getEdgeCost v1 v2 | (v1, v2) <- zip path (tail path)]
  where (graph, root, _, getEdgeCost) = mkGraph input
        path = G.reachable graph root



-- nodes

type Point = (Int, Int)
data Node  = Node { pos :: Point
                  , sym :: Char
                  } deriving (Eq, Ord)

instance Show Node where
  show (Node (row, col) s) = show (row, col, s)

type NodeMap = M.Map Node [(Node, Int)]

mkNode :: Int -> Int -> Char -> Node
mkNode row col = Node (row, col)

mkRowNode, mkColNode :: Int -> Int -> Char -> Node
mkRowNode = mkNode
mkColNode = flip mkNode



-- parsing

processLine :: (Int -> Char -> Node) -> String -> NodeMap -> NodeMap
processLine f s m = res
  where (res, _, _) = foldl' pLine_ (m, Nothing, 0) $ zip [0..] s
        pLine_ (resMap, lastPoint, steps) (i, c)
          | c == '+' || (i == 0 && c == '|') || isLetter c =
            let n2 = f i c in
              case lastPoint of
                Nothing -> (resMap, Just n2, 0)
                Just n1 -> ( insertN n1 n2 steps $ insertN n2 n1 steps resMap
                           , if isLetter c then Just n2 else Nothing
                           , 0
                           )
          | isSpace c = (resMap, Nothing, 0)
          | otherwise = (resMap, lastPoint, steps + 1)
        insertN n1 n2 steps = M.insertWith (++) n1 [(n2, steps + 1)]

mkGraph :: String -> (G.Graph, G.Vertex, G.Vertex -> Char, G.Vertex -> G.Vertex -> Int)
mkGraph input = (graph, root, sym . getNode, getEdgeCost)
  where (graph, getNodeInfo, getVertex) = G.graphFromEdges edges
        getNode v       = let (s, p, _) = getNodeInfo v in Node p s
        getEdgeCost a b = fromJust $ lookup (getNode b) $ nodeMap M.! getNode a
        pRow m (i, s)   = processLine (mkRowNode i) s m
        pCol m (i, s)   = processLine (mkColNode i) s m
        rows            = zip [0..] $ lines input
        cols            = zip [0..] $ transpose $ lines input
        nodeMap         = foldl' pCol (foldl' pRow M.empty rows) cols
        edges           = [(sym n, pos n, pos . fst <$> ns) | (n, ns) <- M.assocs nodeMap]
        root            = fromJust $ getVertex $ head [p | p@(0,_) <- pos <$> M.keys nodeMap]

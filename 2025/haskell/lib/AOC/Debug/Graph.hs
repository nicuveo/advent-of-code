module AOC.Debug.Graph where

import Data.Graph
import Data.Maybe
import Text.Printf


type Name        = String
type VertexLabel = Vertex -> Maybe String
type EdgeLabel   = Edge   -> Maybe String

graphToDot :: Name -> VertexLabel -> EdgeLabel -> Graph -> String
graphToDot name vl el g =
  unlines $ concat [ [printf "digraph %s {" name]
                   , mapMaybe printV (vertices g)
                   , printE <$> edges g
                   , ["}"]
                   ]
  where printV v = printf "  %d [label=\"%s\"];" v <$> vl v
        printE e@(f,t) = case el e of
                           Nothing -> printf "  %d -> %d;" f t
                           Just l  -> printf "  %d -> %d [label=\"%s\"];" f t l

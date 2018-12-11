-- module

module Day07 (day07_1, day07_2) where



-- import

import           Data.Char
import           Data.List
import           Safe
import           Text.Parsec

import           Common



-- solution

day07_1 :: Solution
day07_1 input = show $ topSort edges nodes
  where edges = parseInput input
        nodes = nub $ sort $ concat [[a,b] | (a,b) <- edges]

day07_2 :: Solution
day07_2 input = show $ doAllTheThings edges nodes []
  where edges = parseInput input
        nodes = nub $ sort $ concat [[a,b] | (a,b) <- edges]




-- helpers

testData :: String
testData = "Step C must be finished before step A can begin.\n\
           \Step C must be finished before step F can begin.\n\
           \Step A must be finished before step B can begin.\n\
           \Step A must be finished before step D can begin.\n\
           \Step B must be finished before step E can begin.\n\
           \Step D must be finished before step E can begin.\n\
           \Step F must be finished before step E can begin.\n"

type Node = Char
type Edge = (Node, Node)


completionTime :: Node -> Int
completionTime node = ord node - 4

doAllTheThings :: [Edge] -> [Node] -> [(Int, Node)] -> Int
doAllTheThings _     []    []      = 0
doAllTheThings edges nodes workers = wait + doAllTheThings edges newNodes newWorkers
  where elves = 5
        (wait, node) = headDef (0, chr 0) workers
        newNodes = nodes \\ [node]
        remaining = drop 1 [(r - wait, n) | (r, n) <- workers]
        newWorkers = sort $ take elves $ remaining ++
          [ (completionTime n, n)
          | n <- getPossibleTasks edges newNodes
          , n `notElem` map snd workers
          ]


topSort :: [Edge] -> [Node] -> [Node]
topSort _     []    = []
topSort edges nodes = candidate : topSort edges (nodes \\ [candidate])
  where candidate   = head $ getPossibleTasks edges nodes

getPossibleTasks :: [Edge] -> [Node] -> [Node]
getPossibleTasks edges nodes = nodes \\ [to | (from, to) <- edges, from `elem` nodes]


parseInput :: String -> [Edge]
parseInput = map (parseWith edge) . lines
  where edge = do
          symbol "Step "
          a <- upper
          symbol "must be finished before step "
          b <- upper
          symbol "can begin."
          return (a, b)

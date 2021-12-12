{-# LANGUAGE MultiWayIf #-}

-- import

import           Control.Monad
import           Data.Char
import           Data.Function       (on)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as S
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC


-- input

type Graph = HashMap String [String]

parseGraph :: String -> Graph
parseGraph = M.fromListWith (++) . concat . parseLinesWith line
  where
    line = do
      node1 <- many1 letter
      char '-'
      node2 <- many1 letter
      pure [(node1, [node2]), (node2, [node1])]


-- solution

allPaths :: Graph -> [[String]]
allPaths graph = go S.empty "start"
  where
    go seen node =
      if | node == "end"        -> [["end"]]
         | node `S.member` seen -> []
         | otherwise            ->
           let
             shouldBeAdded = all isLower node
             neighbours = M.findWithDefault [] node graph
             newSeen = if shouldBeAdded then S.insert node seen else seen
             allFurtherPaths = concatMap (go newSeen) neighbours
           in
             map (node:) allFurtherPaths

allPaths2 :: Graph -> [[String]]
allPaths2 graph = go False S.empty "start"
  where
    go usedExtra seen node =
      if | node == "end"        -> [["end"]]
         | node `S.member` seen ->
           if | node == "start" -> []
              | usedExtra       -> []
              | otherwise       -> step True seen node
         | otherwise            -> step usedExtra seen node
    step usedExtra seen node =
      let
        shouldBeAdded = all isLower node
        neighbours = M.findWithDefault [] node graph
        newSeen = if shouldBeAdded then S.insert node seen else seen
        allFurtherPaths = concatMap (go usedExtra newSeen) neighbours
      in
        map (node:) allFurtherPaths

part1 :: Graph -> Int
part1 = length . allPaths

part2 :: Graph -> Int
part2 = length . allPaths2


-- main

main :: IO ()
main = aocMain 12 $ \rawData -> do
  let testInput = parseGraph example
      realInput = parseGraph rawData
  -- let ap2 = allPaths2 $ parseGraph "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"
  -- print $ length ap2
  -- mapM_ print ap2
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW"

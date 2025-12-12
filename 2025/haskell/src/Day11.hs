module Day11 where

import "this" Prelude

import AOC

import Control.Monad.Extra
import Data.HashMap.Strict qualified as M
import Data.Text           qualified as T
import Data.Text.IO        qualified as T
import Text.Dot            hiding (Node, graph, node)
import Text.Dot            qualified as TD
import Text.Parsec
import TextBuilder         qualified as TB


type Graph = HashMap Name Node

data Node = Node
  { nodeName :: Name
  , dataIn   :: [Node]
  , dataOut  :: [Node]
  }

instance Show Node where
  show (Node name ins outs) = concat
    [ "["
    , name
    , " in("
    , intercalate ", " $ map nodeName ins
    , ") out("
    , intercalate ", " $ map nodeName outs
    , ")]"
    ]

type Name = String

startName, exitName :: String
startName = "you"
exitName  = "out"


render :: Graph -> Text
render = TB.toText
  . flip evalState M.empty
  . TD.strictDigraphT
  . traverse visit
  where
    visit (Node name _ children) = do
      lift (gets (M.lookup name)) >>= \case
        Just nodeId ->
          pure nodeId
        Nothing -> do
          nodeId <- TD.node $ T.pack name
          whenJust (M.lookup name specialColors) \colorName -> do
            its style ?= "filled"
            its fillcolor ?= colorName
          lift $ modify (M.insert name nodeId)
          for_ children \child -> do
            childId <- visit child
            nodeId --> childId
          pure nodeId
    specialColors = M.fromList
      [ (startName, "palegreen")
      , (exitName,  "plum")
      , ("fft",     "mediumturquoise")
      , ("svr",     "mediumturquoise")
      , ("dac",     "mediumturquoise")
      ]

findEdges :: Name -> [(Name, Name)] -> ([Name], [Name])
findEdges name = foldl' step ([], [])
  where
    step (edgesIn, edgesOut) (nodeFrom, nodeTo)
      | nodeFrom == name = (edgesIn, nodeTo : edgesOut)
      | nodeTo   == name = (nodeFrom : edgesIn, edgesOut)
      | otherwise        = (edgesIn, edgesOut)

parseInput :: String -> Graph
parseInput = postProcess . unzip . parseLinesWith node
  where
    node = do
      n <- identifier
      symbol ":"
      subNodes <- identifier `sepBy` spaces
      pure (n, map (n, ) subNodes)
    postProcess (names, concat -> edges) =
      let
        graphMap = M.fromList do
          name <- "out" : names
          let (edgesIn, edgesOut) = findEdges name edges
          pure (name, Node name [graphMap M.! e | e <- edgesIn] [graphMap M.! e | e <- edgesOut])
      in
        graphMap


countPaths
  :: Graph
  -> Name
  -> Name
  -> Int
countPaths graph fromName toName =
  flip evalState M.empty $ visit $ graph M.! toName
  where
    visit Node {..} = do
      if nodeName == fromName
      then
        pure 1
      else do
        gets (M.lookup nodeName) >>= \case
          Just result -> pure result
          Nothing -> do
            result <- sum <$> traverse visit dataIn
            modify $ M.insert nodeName result
            pure result


part1 :: Graph -> Int
part1 g = countPaths g "you" "out"

part2 :: Graph -> Int
part2 g = go "fft" "dac" + go "dac" "fft"
  where
    go node1 node2 = product
      [ countPaths g "svr" node1
      , countPaths g node1 node2
      , countPaths g node2 "out"
      ]


example1 :: String
example1 = "\
  \aaa: hhh\n\
  \you: bbb ccc\n\
  \bbb: ddd eee\n\
  \ccc: ddd eee fff\n\
  \ddd: ggg\n\
  \eee: out\n\
  \fff: out\n\
  \ggg: out\n\
  \hhh: ccc fff iii\n\
  \iii: out"

example2 :: String
example2 = "\
  \svr: aaa bbb\n\
  \aaa: fft\n\
  \fft: ccc\n\
  \bbb: tty\n\
  \tty: ccc\n\
  \ccc: ddd eee\n\
  \ddd: hub\n\
  \hub: fff\n\
  \eee: dac\n\
  \dac: fff\n\
  \fff: ggg hhh\n\
  \ggg: out\n\
  \hhh: out"

run :: String -> IO ()
run rawData = do
  let testInput1 = parseInput example1
      testInput2 = parseInput example2
      realInput  = parseInput rawData
  T.writeFile "test_input_1.dot" $ render testInput1
  T.writeFile "test_input_2.dot" $ render testInput2
  T.writeFile "real_input.dot"   $ render realInput
  putStrLn "# Part 1"
  print $ part1 testInput1
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput2
  print $ part2 realInput

module Main where


-- import

import Control.Monad.Loops
import Control.Monad.State
import Data.Foldable
import Data.HashMap.Strict (HashMap, (!), (!?))
import Data.HashMap.Strict qualified as M
import Data.HashSet        (HashSet)
import Data.HashSet        qualified as S
import Data.List           qualified as L
import Data.Maybe
import Data.Tuple
import Text.Parsec
import Text.Parsec.Char
import Text.Printf

import AOC


-- input

type ValveName = String
type DistanceMap = HashMap (ValveName, ValveName) Int
type Input =
  ( HashMap ValveName Int
  , DistanceMap
  )

parseInput :: String -> Input
parseInput = postProcess . parseLinesWith do
  symbol "Valve"
  valve <- valveName
  symbol "has flow rate="
  flowRate <- number
  tryAll
    [ symbol "; tunnel leads to valve"
    , symbol "; tunnels lead to valves"
    ]
  neighbours <- valveName `sepBy` symbol ","
  pure (valve, flowRate, neighbours)
  where
    valveName = lexeme $ many1 letter
    postProcess valves =
      let flowMap = M.fromList do
            (valve, flow, _) <- valves
            guard $ flow > 0
            pure (valve, flow)
          distanceMap = buildDistanceMap $ M.fromList do
            (valve, _, neighbours) <- valves
            pure (valve, neighbours)
       in (flowMap, distanceMap)

buildDistanceMap :: HashMap ValveName [ValveName] -> DistanceMap
buildDistanceMap edges = flip execState mempty $
  iterateUntilM null (fmap concat . traverse step) firstEdges
  where
    firstEdges :: [(ValveName, ValveName, Int)]
    firstEdges = do
      (start, ends) <- M.toList edges
      end <- ends
      pure (start, end, 1)
    step
      :: MonadState DistanceMap m
      => (ValveName, ValveName, Int)
      -> m [(ValveName, ValveName, Int)]
    step (start, end, cost) =
      gets (M.lookup (start, end)) >>= \case
        Just _  -> pure []
        Nothing -> do
          modify $ M.insert (start, end) cost
          pure do
            nextDestination <- edges M.! end
            pure (start, nextDestination, cost+1)


-- solution

visitValves
  :: Int
  -> Input
  -> HashMap (HashSet ValveName) Int
visitValves totalTime (flowMap, distanceMap) = L.foldl1' (M.unionWith max) do
  startValve <- M.keys flowMap
  let startTime = totalTime - fromMaybe 0 (distanceMap !? ("AA", startValve))
  pure $ go 0 startTime startValve (S.singleton startValve)
  where
    go totalFlow timeLeft valve seen =
      let
        updatedFlow = totalFlow + (flowMap ! valve) * (timeLeft - 1)
        nextValves = do
          targetValve <- M.keys flowMap
          guard $ not $ targetValve `S.member` seen
          let targetTimeLeft = timeLeft - 1 - (distanceMap ! (valve, targetValve))
          guard $ targetTimeLeft > 0
          pure $ go updatedFlow targetTimeLeft targetValve (S.insert targetValve seen)
        stopHere = M.singleton seen updatedFlow
      in L.foldl' (M.unionWith max) stopHere nextValves

part1 :: Input -> Int
part1 = maximum . visitValves 30

part2 :: Input -> Int
part2 = maximum . bestCombo . visitValves 26
  where
    bestCombo (M.toList -> paths) = do
      (seen1, flow1) : rest <- L.tails paths
      (seen2, flow2) <- rest
      guard $ S.null $ S.intersection seen1 seen2
      pure $ flow1 + flow2


-- main

main :: IO ()
main = aocMain 16 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"

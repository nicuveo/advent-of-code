module Main where


-- import

import Control.Monad.Loops
import Control.Monad.State
import Data.Foldable
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as M
import Data.HashSet        qualified as HS
import Data.List           qualified as L
import Data.Maybe
import Data.Set            qualified as S
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
  :: Input
  -> [(ValveName, Int)]
  -> [(String, Int)]
visitValves (flowMap, distanceMap) start = go
  (  S.fromList $ swap <$> start
  , HS.fromList $ fst  <$> start
  )
  where
    go (targets, seen)
      | S.null targets = []
      | otherwise =
        let
          ((timeLeft, valve), remainingTargets) = S.deleteFindMax targets
          baseResult = (flowMap ! valve) * (timeLeft - 1)
          subBranches = do
            targetValve <- M.keys flowMap
            guard $ not $ targetValve `HS.member` seen
            let distance = fromJust $ M.lookup (valve, targetValve) distanceMap
                targetTimeLeft = timeLeft - 1 - distance
            guard $ targetTimeLeft > 0
            pure
              ( S.insert (targetTimeLeft, targetValve) remainingTargets
              , HS.insert targetValve seen
              )
          nextSteps = if null subBranches
            then [(remainingTargets, seen)]
            else subBranches
          subResults = concatMap go nextSteps
        in if null subResults
           then [(valve, baseResult)]
           else do
             (path, cost) <- subResults
             pure ( valve ++ " -> " ++ path
                  , cost + baseResult
                  )

part1 :: Input -> Int
part1 input@(flowMap, distanceMap) = maximum $ map snd do
  valve <- M.keys flowMap
  let timeLeft = 30 - fromMaybe 0 (distanceMap M.!? ("AA", valve))
  visitValves input [(valve, timeLeft)]

part2 :: Input -> Int
part2 input@(flowMap, distanceMap) = maximum $ map snd do
  (valve1 : others) <- L.tails $ M.keys flowMap
  valve2 <- others
  let timeLeft1 = 26 - fromMaybe 0 (distanceMap M.!? ("AA", valve1))
      timeLeft2 = 26 - fromMaybe 0 (distanceMap M.!? ("AA", valve2))
  visitValves input [(valve1, timeLeft1), (valve2, timeLeft2)]


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

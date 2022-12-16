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

part1 :: Input -> Int
part1 (flowMap, distanceMap) = maximum do
  valve <- M.keys flowMap
  guard $ flowMap M.! valve > 0
  maybeToList $ go
    mempty
    valve
    (30 - fromMaybe 0 (distanceMap M.!? ("AA", valve)))
  where
    go seen valve !timeLeft
      | valve `S.member` seen = Nothing
      | timeLeft <= 0 = Nothing
      | otherwise =
        let baseResult = (flowMap ! valve) * (timeLeft - 1)
            nextSteps = do
              nextValve <- M.keys flowMap
              guard $ flowMap M.! nextValve > 0
              let distance =
                    fromMaybe
                    (error $ printf "no path found between %s and %s" valve nextValve)
                    (M.lookup (valve, nextValve) distanceMap)
              maybeToList $ go
                (S.insert valve seen)
                nextValve
                (timeLeft - 1 - distance)
         in Just $ if null nextSteps
                   then baseResult
                   else baseResult + maximum nextSteps

part2 :: Input -> Int
part2 (flowMap, distanceMap) = maximum do
  (valve1 : others) <- L.tails $ M.keys flowMap
  guard $ flowMap M.! valve1 > 0
  valve2 <- others
  guard $ flowMap M.! valve2 > 0
  let time1 = 26 - fromMaybe 0 (distanceMap M.!? ("AA", valve1))
      time2 = 26 - fromMaybe 0 (distanceMap M.!? ("AA", valve2))
      seen = HS.fromList [valve1, valve2]
      targets = S.fromList [(time1, valve1), (time2, valve2)]
  (_, cost) <- step targets seen
  pure cost
  where
    step targets seen
      | S.null targets = []
      | otherwise =
        let ((nextTime, nextValve), nextTargets) = S.deleteFindMax targets
        in go nextTargets seen nextTime nextValve
    go targets seen timeLeft valve
      | timeLeft <= 0 = []
      | otherwise =
        let baseResult = (flowMap ! valve) * (timeLeft - 1)
            subBranches = do
              targetValve <- M.keys flowMap
              guard $ flowMap M.! targetValve > 0
              guard $ not $ targetValve `HS.member` seen
              let distance = fromJust $ M.lookup (valve, targetValve) distanceMap
                  targetTimeLeft = timeLeft - 1 - distance
              pure
                ( S.insert (targetTimeLeft, targetValve) targets
                , HS.insert targetValve seen
                )
            nextSteps = if null subBranches
              then [(targets, seen)]
              else subBranches
            subResults = concatMap (uncurry step) nextSteps
        in if null subResults
           then [(valve, baseResult)]
           else do
             (path, cost) <- subResults
             pure ( valve ++ " -> " ++ path
                  , cost + baseResult
                  )

{-
    go seen valve !timeLeft
      | valve `S.member` seen = []
      | timeLeft <= 0 = []
      | otherwise =
        let baseResult = (flowMap ! valve) * (timeLeft - 1)
            nextSteps = do
              nextValve <- M.keys flowMap
              guard $ flowMap M.! nextValve > 0
              let distance =
                    fromMaybe
                    (error $ printf "no path found between %s and %s" valve nextValve)
                    (M.lookup (valve, nextValve) distanceMap)
              go
                (S.insert valve seen)
                nextValve
                (timeLeft - 1 - distance)
         in ([valve], baseResult) : do
              (path, cost) <- nextSteps
              pure ([valve] ++ path, cost + baseResult)
-}

-- main

main :: IO ()
main = aocMain 16 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  -- for_ (L.sortOn snd $ part1 testInput) \(path, cost) -> do
  --   printf "%4d %s\n" cost $ L.intercalate ", " path
  -- print $ part1 testInput
  -- print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"

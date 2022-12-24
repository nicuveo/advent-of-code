module Main where


-- import

import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import Data.HashMap.Strict (HashMap, (!))
import Data.HashMap.Strict qualified as M
import Data.HashSet        (HashSet)
import Data.HashSet        qualified as S
import Data.Maybe
import Safe

import AOC


-- input

type Elves = HashSet Point

parseInput :: String -> Elves
parseInput input = S.fromList do
  (y, r) <- zip [50..] $ lines input
  (x, c) <- zip [50..] r
  guard $ c == '#'
  pure $ Point x y


-- solution

data FieldState = FieldState
  { fsCycle :: [[Vector]]
  , fsElves :: Elves
  , fsRound :: Int
  }

run :: State FieldState a -> Elves -> a
run action elves = evalState action (FieldState directionCycle elves 1)

directionCycle :: [[Vector]]
directionCycle = cycle $ map (map directionVector)
  [ [N, NW, NE]
  , [S, SW, SE]
  , [W, NW, SW]
  , [E, NE, SE]
  ]

suggest :: MonadState FieldState m => Point -> m (Maybe (Point, Point))
suggest elf = do
  FieldState {..} <- get
  let eightNeighbs = eightSurroundingPoints elf
  if not $ any (`S.member` fsElves) eightNeighbs
  then pure Nothing
  else pure $ headMay do
    points@(candidate:_) <- map (map (elf+)) $ take 4 fsCycle
    guard $ not $ any (`S.member` fsElves) points
    pure (elf, candidate)

step :: MonadState FieldState m => m (Maybe Int)
step = do
  FieldState {..} <- get
  targets <- catMaybes <$> traverse suggest (S.toList fsElves)
  let unique = M.keysSet $ M.mapMaybe id $ M.fromListWith reject [(t, Just f) | (f, t) <- targets]
      moves  = M.fromList [(f, t) | (f, t) <- targets, t `S.member` unique]
  if M.null moves
  then pure $ Just fsRound
  else do
    put $ FieldState
      { fsCycle = tail fsCycle
      , fsElves = S.map (\elf -> fromMaybe elf $ M.lookup elf moves) fsElves
      , fsRound = succ fsRound
      }
    pure Nothing
  where
    reject _ _ = Nothing

boundingBox :: MonadState FieldState m => m (Point, Point)
boundingBox = do
  elves <- S.toList <$> gets fsElves
  let minX = minimum $ map px elves
      minY = minimum $ map py elves
      maxX = maximum $ map px elves
      maxY = maximum $ map py elves
  pure (Point minX minY, Point maxX maxY)


part1 :: Elves -> Int
part1 = run do
  replicateM_ 10 step
  (Point minX minY, Point maxX maxY) <- boundingBox
  elves <- gets fsElves
  pure $ (maxX - minX + 1) * (maxY - minY + 1) - S.size elves

part2 :: Elves -> Int
part2 = run $ untilJust step


-- main

main :: IO ()
main = aocMain 23 \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "....#..\n..###.#\n#...#.#\n.#...##\n#.###..\n##.#.##\n.#..#.."

module Day16 where


-- import

import AOC
import AOC.Grid.Flat
import "this" Prelude

import Control.Monad.Loops (iterateUntilM)
import Data.HashSet        qualified as S


-- input

type Input = Grid Char

parseInput :: String -> Input
parseInput = from2DList . lines


-- solution

type OrientedPoint = (Point, Direction)
type SolveState    = HashSet OrientedPoint
type SolveMonad m  = (MonadReader Input m, MonadState SolveState m)

inspect :: SolveMonad m => Point -> m Char
inspect p = asks (! p)

step :: SolveMonad m => OrientedPoint -> m [OrientedPoint]
step (p, d) = do
  directions <-
    inspect p <&> \case
      '.'  -> [d]
      '/'  -> case d of
        N -> [E]
        W -> [S]
        S -> [W]
        E -> [N]
        _ -> error "encountered a diagonal!"
      '\\' -> case d of
        N -> [W]
        W -> [N]
        S -> [E]
        E -> [S]
        _ -> error "encountered a diagonal!"
      '|'  -> case d of
        W -> [N, S]
        E -> [N, S]
        o -> [o]
      '-'  -> case d of
        N -> [W, E]
        S -> [W, E]
        o -> [o]
      c -> error $ "unexpected character '" ++ [c] ++ "' in input"
  grid <- ask
  previous <- get
  let result = do
        d' <- directions
        let p' = p + directionVector d'
        guard $ inBounds grid p'
        guard $ not $ S.member (p', d') previous
        pure (p', d')
  modify $ S.union (S.fromList result)
  pure result

energize :: SolveMonad m => OrientedPoint -> m ()
energize p = void do
  put $ S.singleton p
  iterateUntilM null go [p]
  where
    go = fmap concat . traverse step

run :: Input -> ReaderT Input (State SolveState) a -> a
run input action = evalState (runReaderT action input) S.empty


part1 :: Input -> Int
part1 input = run input do
  energize (Point 0 0, E)
  gets $ S.size . S.map fst

part2 :: Input -> Int
part2 input = run input do
  grid <- ask
  let w = gWidth  grid
      h = gHeight grid
      startingPoints = concat
        [ [(Point x     0,     S) | x <- [0..w-1]]
        , [(Point 0     y,     E) | y <- [0..h-1]]
        , [(Point x     (h-1), N) | x <- [0..w-1]]
        , [(Point (w-1) y,     W) | y <- [0..h-1]]
        ]
  fmap maximum $
    for startingPoints \p -> do
      energize p
      gets $ S.size . S.map fst


-- main

example :: String
example = ".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|...."

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn $ displayWith (const pure) testInput
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

{-# LANGUAGE TemplateHaskell #-}

module Day06 where


-- import

import AOC
import "this" Prelude

import Control.Lens
import Control.Monad.Extra         (whileM)
import Control.Monad.Writer.Strict qualified as W
import Data.HashSet                qualified as S


-- input

data Input = Input
  { obstacles     :: HashSet Point
  , startingPoint :: Point
  , mapWidth      :: Int
  , mapHeight     :: Int
  } deriving Show

parseInput :: String -> Input
parseInput rawInput = Input (S.fromList $ catMaybes os) (head sp) w h
  where
    h = length $ lines rawInput
    w = length $ head $ lines $ rawInput
    (os, sp) = W.runWriter $ sequence do
      (y, row) <- zip [0..] $ lines rawInput
      (x, c)   <- zip [0..] row
      pure do
        when (c == '^') $ W.tell [Point x y]
        pure $ if c == '#' then Just (Point x y) else Nothing


-- solution

data WalkState = WalkState
  { _currentPosition  :: Point
  , _currentDirection :: Direction
  , _knownPositions   :: HashSet (Point, Direction)
  }

makeLenses ''WalkState

startingState :: Point -> WalkState
startingState p = WalkState p N (S.singleton (p, N))

step
  :: (MonadState WalkState m, MonadReader Input m)
  => m Bool
step = do
  WalkState pos dir _   <- get
  Input obs _ maxW maxH <- ask
  let tentativePosition = pos + directionVector dir
      outOfBounds = or
        [ px tentativePosition < 0
        , px tentativePosition >= maxW
        , py tentativePosition < 0
        , py tentativePosition >= maxH
        ]
      facingObstacle = S.member tentativePosition obs
  if | outOfBounds    -> pure False
     | facingObstacle -> do
         currentDirection %= turn90R
         pure True
     | otherwise      -> do
         currentPosition .= tentativePosition
         knownPositions  %= S.insert (tentativePosition, dir)
         pure True

isLoop
  :: (MonadState WalkState m, MonadReader Input m)
  => m Bool
isLoop = do
  seen     <- use knownPositions
  inBounds <- step
  if not inBounds
  then pure False
  else do
    pos <- use currentPosition
    dir <- use currentDirection
    if (pos, dir) `S.member` seen
    then pure True
    else isLoop

allPositions :: Input -> HashSet Point
allPositions input = S.map fst
  $ _knownPositions
  $ flip execState (startingState $ startingPoint input)
  $ flip runReaderT input
  $ whileM step

part1 :: Input -> Int
part1 = S.size . allPositions

part2 :: Input -> Int
part2 input = countTrue do
  p <- S.toList allPoints
  guard $ p /= startingPoint input
  let modifiedInput = input { obstacles = S.insert p $ obstacles input }
  pure
    $ flip evalState (startingState $ startingPoint input)
    $ flip runReaderT modifiedInput
    $ isLoop
  where
    allPoints = allPositions input


-- main

example :: String
example = "\
\....#.....\n\
\.........#\n\
\..........\n\
\..#.......\n\
\.......#..\n\
\..........\n\
\.#..^.....\n\
\........#.\n\
\#.........\n\
\......#..."

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

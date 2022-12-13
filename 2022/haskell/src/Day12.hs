module Main where


-- import

import Control.Monad
import Control.Monad.Loops  (untilJust)
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.Foldable
import Data.HashMap.Strict  qualified as M
import Data.PQueue.Min      as Q
import Text.Printf

import AOC
import AOC.Grid.Flat

import Debug.Trace


-- input

type Input = (Grid Int, Point, Point)

parseInput :: String -> Input
parseInput s =
  let charMap = from2DList $ lines s
   in ( fmap toElevation charMap
      , findPoint 'S' charMap
      , findPoint 'E' charMap
      )
  where
    toElevation c
      | isLower c = ord c - ord 'a'
      | c == 'S'  = 0
      | c == 'E'  = 25
      | otherwise = error "unrecognized input!"
    findPoint c m = head do
      p <- allPoints m
      guard $ m ! p == c
      pure p


-- solution

part1 :: Input -> Int
part1 (g, s, e) =
  maybe (error "no path found!") ((subtract 1) . length) $
  findPath nextSteps s e
  where
    nextSteps p = do
      candidate <- gridFourSurroundingPoints g p
      guard $ (g ! candidate) <= (g ! p) + 1
      pure (1, candidate)

data FloodState = FloodState
  { fsQueue    :: Q.MinQueue (Int, Point)
  , fsNodeInfo :: M.HashMap Point Int
  }

initialState :: Point -> FloodState
initialState s = FloodState
  (Q.singleton (0, s))
  (M.singleton s 0)

getElevation
  :: MonadReader (Grid Int) m
  => Point
  -> m Int
getElevation p = asks (! p)

step
  :: (MonadReader (Grid Int) m, MonadState FloodState m)
  => m (Maybe Int)
step = do
  FloodState queue nodeInfo <- get
  let ((distance, point), newQueue) = Q.deleteFindMin queue
  currentElevation <- getElevation point
  if currentElevation == 0
  then pure (Just distance)
  else do
    put $ FloodState newQueue nodeInfo
    g <- ask
    let nextPoints = gridFourSurroundingPoints g point
    for_ nextPoints \nextPoint -> do
      newElevation <- getElevation nextPoint
      let newDistance = distance + 1
          isBetter = maybe True (newDistance <) $ M.lookup nextPoint nodeInfo
      when (isBetter && newElevation >= currentElevation - 1) do
        modify \s -> s
          { fsQueue = Q.insert (newDistance, nextPoint) (fsQueue s)
          , fsNodeInfo = M.insert nextPoint newDistance (fsNodeInfo s)
          }
    pure Nothing

part2 :: Input -> Int
part2 (g, _, s) =
  flip runReader g $
    flip evalStateT (initialState s) $
      untilJust step


-- main

main :: IO ()
main = aocMain 12 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"

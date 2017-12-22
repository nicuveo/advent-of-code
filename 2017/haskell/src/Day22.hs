{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}



-- module

module Day22 ( day22_1, day22_2
             , printState
             ) where



-- import

import           Control.Monad.State.Strict
import           Common
import qualified Data.Map.Strict as M



-- solution

day22_1 :: Solution
day22_1 input = show $ inf $ evalState (stepsN 10000 nextState virus) world
  where (world, virus) = parseOriginalState input
        nextState Clean    = Infected
        nextState Infected = Clean
        nextState _        = error "day22: should not encounter a state other than Clean or Infected in part1."

day22_2 :: Solution
day22_2 input = show $ inf $ evalState (stepsN 10000000 nextState virus) world
  where (world, virus) = parseOriginalState input
        nextState Clean    = Weakened
        nextState Weakened = Infected
        nextState Infected = Flagged
        nextState Flagged  = Clean



-- types

type Point = (Int, Int)

data NodeState = Clean
               | Weakened
               | Infected
               | Flagged
               deriving (Show, Eq)

type StateFunction = NodeState -> NodeState

type World = M.Map Point NodeState

data Virus = Virus { pos :: Point
                   , dir :: Point
                   , inf :: Int
                   } deriving Show



-- point functions

plus :: Point -> Point -> Point
plus (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

up :: Point
up = (-1, 0)

turnR :: Point -> Point
turnR (r, c) = (c, -r)

turnL :: Point -> Point
turnL (r, c) = (-c, r)



-- world functions

getState :: MonadState World m => Point -> m NodeState
getState p = M.findWithDefault Clean p <$> get

setState :: MonadState World m => Point -> NodeState -> m ()
setState p s = M.insert p s <$> get >>= put



-- virus functions

step :: MonadState World m => StateFunction -> Virus -> m Virus
step nextState !virus = do
  localState <- getState $ pos virus
  let newDir   = dirTransform localState $ dir virus
      newState = nextState localState
  setState (pos virus) newState
  return $ virus { pos = pos virus `plus` newDir
                 , dir = newDir
                 , inf = inf virus + fromEnum (newState == Infected)
                 }
  where dirTransform Clean    = turnL
        dirTransform Weakened = id
        dirTransform Infected = turnR
        dirTransform Flagged  = turnR . turnR

stepsN :: MonadState World m => Int -> StateFunction -> Virus -> m Virus
stepsN  1 f v = step f v
stepsN !n f v = step f v >>= stepsN (n-1) f



-- debugging

printState :: World -> Virus -> IO ()
printState w v = do
  print v
  mapM_ putStrLn [ concat [ if (r,c) == pos v
                            then '[' : char (r,c) : "]"
                            else ' ' : char (r,c) : " "
                          | c <- [minX .. maxX]
                          ]
                 | r <- [minY .. maxY]
                 ]
  where knownPos     = M.keys w
        (minY, maxY) = (minimum $ fst <$> knownPos, maximum $ fst <$> knownPos)
        (minX, maxX) = (minimum $ snd <$> knownPos, maximum $ snd <$> knownPos)
        char p       = case M.findWithDefault Clean p w of
                         Clean    -> '.'
                         Weakened -> 'W'
                         Infected -> '#'
                         Flagged  -> 'F'



-- parsing

parseOriginalState :: String -> (World, Virus)
parseOriginalState input = ( M.fromAscList [ ((ri, ci), Infected)
                                           | (ri, rt) <- zip [0..] l
                                           , (ci, cc) <- zip [0..]  rt
                                           , cc == '#'
                                           ]
                           , Virus (div h 2, div w 2) up 0
                           )
  where l = lines input
        h = length l
        w = length $ head l

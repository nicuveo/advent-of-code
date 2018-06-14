{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}


-- module

module Day13 (day13_1, day13_2, printMaze) where



-- import

import           Control.Monad
import           Control.Monad.State
import           Data.Bits
import qualified Data.IntMap         as M
import           Data.Maybe
import qualified Data.PQueue.Min     as Q
import qualified Data.Sequence       as S
import qualified Data.Set            as U

import           Common



-- solution

day13_1 :: Solution
day13_1 input = "from (1,1) to (31,39): " ++ show (length path - 1) ++ " steps"
  where salt = read input
        path = eval salt $ findPath (1,1) (31,39)


day13_2 :: Solution
day13_2 input  = "from (1,1) in <= 50 steps: " ++ show (length points)
  where salt   = read input
        points = eval salt $ findReachable (1,1) 50



-- maze

type Point   = (Int, Int)
data Env     = Env { envSalt :: Int
                   , envMaze :: M.IntMap Bool
                   }

type Context = State Env


mkEnv :: Int -> Env
mkEnv = flip Env M.empty

eval :: Int -> Context a -> a
eval salt = flip evalState $ mkEnv salt

mazeSize :: Int
mazeSize = 1000

pointToKey :: Point -> Int
pointToKey (x, y) = y * mazeSize + x

layout :: Int -> Point -> Bool
layout salt (x, y) = even $ popCount $ x*x + 3*x + 2*x*y + y + y*y + salt

isEmpty :: Point -> Context Bool
isEmpty point = do
  env <- get
  val <- do
    let v = layout (envSalt env) point
    put $ Env (envSalt env) $ M.insert key v $ envMaze env
    return v
  return $ fromMaybe val $ M.lookup key $ envMaze env
  where key = pointToKey point



-- path finding

getNextPoints :: Point -> Context [Point]
getNextPoints (x, y) = filterM check [ (x-1,y)
                                     , (x,y-1)
                                     , (x+1,y)
                                     , (x,y+1)
                                     ]
  where check point@(px,py) = do
          empty <- isEmpty point
          return $ px >= 0 && py >= 0 && empty

heuristic :: Point -> Point -> Int
heuristic (px,py) (dx,dy) = abs (dx-px) + abs (dy-py)

findReachable :: Point -> Int -> Context [Point]
findReachable start limit = findReachable_ originalSet originalQueue
  where originalSet   = U.fromList [start]
        originalQueue = S.viewl $ S.fromList [(0, start)]
        findReachable_ !seen S.EmptyL = return $ U.elems seen
        findReachable_ !seen ((cost, current) S.:< rest)
          | cost == limit = return $ U.elems seen
          | otherwise     = do
              reachable <- getNextPoints current
              let nexts    = [ next
                             | next <- reachable
                             , next `U.notMember` seen
                             ]
                  newSeen  = U.union seen $ U.fromList nexts
                  newQueue = S.viewl $ rest S.>< S.fromList (map (cost+1,) nexts)
              findReachable_ newSeen newQueue

findPath :: Point -> Point -> Context [Point]
findPath start end = findPath_ originalMap originalQueue
  where originalMap   = M.fromList [(pointToKey start, (0,start))]
        originalQueue = Q.fromList [(0,0,start)]
        construct s seen
          | s == start = [start]
          | otherwise  = s : construct (snd $ seen M.! pointToKey s) seen
        findPath_ !seen !queue
          | end == current = return $ reverse $ construct end seen
          | otherwise      = do
              reachable <- getNextPoints current
              let nexts = [ next
                          | next <- reachable
                          , maybe True (cost+1 <) $ fmap fst $ M.lookup (pointToKey next) seen
                          ]
                  newQueue = Q.union rest $ Q.fromList [(cost+1+heuristic next end, cost+1, next) | next <- nexts]
                  newSeen  = M.union (M.fromList [(pointToKey next,(cost+1,current)) | next <- nexts]) seen
              findPath_ newSeen newQueue
          where ((_,cost,current), rest) = Q.deleteFindMin queue



-- debug

printMaze :: Int -> Int -> String
printMaze size salt = unlines [ [ if layout salt (x,y)
                                  then ' '
                                  else '#'
                                | x <- [0 .. size - 1]
                                ]
                              | y <- [0 .. size - 1]
                              ]

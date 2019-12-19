{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE TupleSections    #-}


-- import

import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Char
import           Data.Function.Memoize
import qualified Data.HashMap.Strict         as M
import           Data.List
import           Data.Maybe
import qualified Data.Set                    as S
import           Data.Tuple

import           AOC
import           AOC.Map.Flat



-- maze

data Maze = Maze { grid :: FlatMap Char
                 , pos  :: M.HashMap Char Point
                 }
type Key           = Char
type DistanceCache = M.HashMap (Key,Key) Int
type Prerequisites = M.HashMap Key [Key]


parseInput :: String -> Maze
parseInput s = Maze g p
  where g = from2DList $ lines s
        p = foldl' i M.empty $ allPoints g
        i m x = if g ! x `elem` "#."
                then m
                else M.insertWith (error "duplicate element") (g ! x) x m

getKeys :: Maze -> [Key]
getKeys = sort . filter isLower . M.keys . pos

getNeighbours :: Maze -> Point -> [Point]
getNeighbours m p = [ n
                    | n <- fourNeighbouringPointsOf (grid m) p
                    , grid m ! n /= '#'
                    ]


computeCost :: Maze -> Point -> Point -> Maybe Int
computeCost m s e = snd . last <$> maybeFindPathH neighbours heuristic s e
  where neighbours p = (1,) <$> getNeighbours m p
        heuristic  p = manhattanNorm $ e - p

findPrerequisites :: Maze -> Prerequisites
findPrerequisites m = flood ([], S.empty, [], []) [pos m M.! '@']
  where flood (od,sp,ks,ds) [] =
          M.unions $ M.fromList ((,sort od) <$> ks) :
              [ flood (toLower d:od,sp,[],[]) nbs
              | d <- ds
              , let nbs = getNeighbours m $ pos m M.! d
              ]
        flood (od,sp,ks,ds) (p:ps) =
          let x   = grid m ! p
              nps = ps ++ getNeighbours m p
              nsp = S.insert p sp
          in if | S.member p sp -> flood (od, sp,  ks,  ds)  ps
                | isUpper x     -> flood (od,nsp,  ks,x:ds)  ps
                | isLower x     -> flood (od,nsp,x:ks,  ds) nps
                | otherwise     -> flood (od,nsp,  ks,  ds) nps

makeCache :: Maze -> DistanceCache
makeCache m = M.fromList $ do
  let keys = getKeys m
  ka <- "@1234" ++ keys
  kb <- "@1234" ++ keys
  guard $ kb > ka
  Just pa <- [M.lookup ka $ pos m]
  Just pb <- [M.lookup kb $ pos m]
  Just p  <- [computeCost m pa pb]
  return ((ka,kb), p)



-- solution

findShortestPath :: Maze -> Prerequisites -> DistanceCache -> [Key] -> Int
findShortestPath m reqs cache = fsp []
  where keys = getKeys m
        wfsp = memoize2 fsp
        fsp ks rs
          | sort ks == keys = 0
          | otherwise = minimum $ withStrategy (parList rseq) $ do
              nk <- keys \\ ks
              guard $ all (`elem` ks) $ reqs M.! nk
              ok <- rs
              let ci = if ok < nk then (ok,nk) else (nk,ok)
              Just cost <- [M.lookup ci cache]
              return $ cost + wfsp (sort $ nk : ks) (sort $ nk : (rs \\ [ok]))



-- main

main :: IO ()
main = aocMain 18 $ \rawInput -> do
  let maze1  = parseInput rawInput
      reqs   = findPrerequisites maze1
      cache1 = makeCache maze1
      start  = pos maze1 M.! '@'
      reps   = zip (eightNeighboursOf start) "1#2##3#4"
      maze2  = maze1 {
        grid = pmap (\p x -> fromMaybe x $ lookup p reps) $ grid maze1,
        pos  = M.union (M.fromList $ swap <$> reps) $ pos maze1
      }
      cache2 = makeCache maze2

  putStrLn $ "Part1: " ++ show (findShortestPath maze1 reqs cache1 "@")
  putStrLn $ "Part2: " ++ show (findShortestPath maze2 reqs cache2 "1234")

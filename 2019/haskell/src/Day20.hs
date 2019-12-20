{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}


-- import

import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Map          as M
import           Data.Maybe

import           AOC
import           AOC.Debug.Animate
import           AOC.Debug.Color
import           AOC.Map.Flat



-- maze

type Grid = FlatMap Char
data Maze = Maze { grid    :: Grid
                 , portals :: M.Map Point (Point, Int)
                 , start   :: Point
                 , end     :: Point
                 }

parseGrid :: String -> Grid
parseGrid s = from2DList f
  where l = lines s
        m = maximum $ length <$> l
        f = take m . (++ repeat ' ') <$> l

extractLabels :: Grid -> M.Map String [(Point,Int)]
extractLabels g = foldr ($) M.empty $ do
  p <- allPoints g
  guard $ g ! p == '.'
  let e  = fromMaybe ' ' . (g !?)
      mx = fmapWidth  g `div` 2
      my = fmapHeight g `div` 2
      ul = e <$> [above   $ above  p, above  p]
      ll = e <$> [leftOf  $ leftOf p, leftOf p]
      dl = e <$> [below   p, below   $ below   p]
      rl = e <$> [rightOf p, rightOf $ rightOf p]
  if | all isUpper ul -> if py p < my
                         then [M.insertWith (++) ul [(p,1)]]
                         else [M.insertWith (++) ul [(p,0)]]
     | all isUpper ll -> if px p < mx
                         then [M.insertWith (++) ll [(p,1)]]
                         else [M.insertWith (++) ll [(p,0)]]
     | all isUpper dl -> if py p > my
                         then [M.insertWith (++) dl [(p,1)]]
                         else [M.insertWith (++) dl [(p,0)]]
     | all isUpper rl -> if px p > mx
                         then [M.insertWith (++) rl [(p,1)]]
                         else [M.insertWith (++) rl [(p,0)]]
     | otherwise      -> []

makePortals :: M.Map String [(Point,Int)] -> M.Map Point (Point,Int)
makePortals l = M.fromList $ concat [ [ (p1, (p2, d2-d1))
                                      , (p2, (p1, d1-d2))
                                      ]
                                    | [(p1,d1),(p2,d2)] <- M.elems l
                                    ]

makeMaze :: String -> Maze
makeMaze m = Maze g p s e
  where g = parseGrid m
        l = extractLabels g
        p = makePortals l
        [(s,1)] = l M.! "AA"
        [(e,1)] = l M.! "ZZ"

openNeighbours :: Grid -> Point -> [Point]
openNeighbours g p = [q | q <- fourNeighbouringPointsOf g p, g ! q == '.']



-- part 1

part1 :: Maze -> Int
part1 (Maze g z s e) = snd $ last $ findPath neighbs s e
  where neighbs p = map (1,) $ maybeToList (fst <$> z M.!? p) ++ openNeighbours g p



-- part 2

type DistanceCache = M.Map Point [(Point, Int)]
type Depth = Int

makeCache :: Maze -> DistanceCache
makeCache (Maze g z s e) = M.fromListWith (++) $ do
  (p1 : p2s) <- tails $ nub $ sort $ s : e : M.keys z
  p2 <- p2s
  Just path <- [maybeFindPathH neighbs (heuristic p2) p1 p2]
  let cost = snd $ last path
  [ (p1, [(p2,cost)]), (p2, [(p1,cost)]) ]
  where neighbs = map (1,) . openNeighbours g
        heuristic = manhattanNorm ... (-)

neighbours :: Maze -> DistanceCache -> (Point,Depth) -> [(Int, (Point,Depth))]
neighbours m c (p,d) = do
  (q,x) <- fromMaybe [] $ c M.!? p
  case portals m M.!? q of
    Nothing    -> return (x, (q,d))
    Just (z,j) -> do
      guard $ d + j >= 0 && d + j < 100
      return (x+1, (z,d+j))

part2 :: Maze -> Int
part2 m = snd $ last $ findPathH (neighbours m cache) heuristic (start m,0) (end m,0)
  where cache = makeCache m
        minDist = minimum $ snd <$> concat (M.elems cache)
        heuristic (_,d) = d * minDist



-- main

animateCache :: Maze -> DistanceCache -> IO ()
animateCache m c = animate 1000 resetCursor render step ([], 0)
  where render x = flip displayWith (grid m) $ \p w -> if elem p $ paths !! x
                                                          then fgColor green "O"
                                                          else pure w
        step x = if x < length paths - 1 then Just ([], x+1) else Nothing
        paths = do
          (p1, p2) <- nub $ sort [ if p2 > p1 then (p1,p2) else (p2,p1)
                                 | (p1, p2s) <- M.assocs c
                                 , (p2, _) <- p2s
                                 ]
          return $ fst <$> findPath neighbs p1 p2
        neighbs = map (1,) . openNeighbours (grid m)

main :: IO ()
main = aocMain 20 $ \rawInput -> do
  let m = makeMaze rawInput
  print $ part1 m
  print $ part2 m

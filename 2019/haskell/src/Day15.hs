{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}


-- import

import           Control.Lens        hiding (Empty)
import           Control.Monad.Extra
import           Control.Monad.State
import           Data.Bool
import           Data.List.Split
import qualified Data.Map.Strict     as M
import           Data.Ratio
import qualified Data.Set            as S
import qualified Data.Vector.Unboxed as V
import           System.IO

import           AOC
import           AOC.Debug.Animate
import           AOC.Debug.Color
import           AOC.Map.Sparse
import           IntCode



-- input

type Program = V.Vector Int

parseInput :: String -> Program
parseInput = V.fromList . map read . splitOn ","



-- maze

type Tile = Bool
data Maze = Maze { _tiles :: SparseMap Tile
                 , _stack :: [(Point, Direction)]
                 , _goal  :: Point
                 , _robot :: Point
                 }
makeLenses ''Maze

type MazeMonad = StateT Maze IO


inF :: MazeMonad (Either ErrorCode Int)
inF = do
  s <- use stack
  r <- use robot
  t <- use tiles
  case s of
    [] -> return $ Left EndOfProgram
    ((p, d) : rest) ->
      if r /= p
      then return $ Right $ goTo r p
      else do
        let nextPoint = r + directionVector d
        if M.member nextPoint t
          then stack .= rest >> inF
          else return $ Right $ directionValue d
  where directionValue N = 1
        directionValue S = 2
        directionValue W = 3
        directionValue E = 4
        directionValue _ = error "wrong direction: tried to move diagonally?"
        goTo p1 p2 = case p2 - p1 of
          Point (-1)   0  -> directionValue N
          Point   1    0  -> directionValue S
          Point   0  (-1) -> directionValue W
          Point   0    1  -> directionValue E
          _               -> error "top point of the stack not a neighbour"

outF :: Int -> MazeMonad ()
outF v = do
  r <- use robot
  (p, d) <- uses stack head
  let newPoint = p + directionVector d
  case (v, p /= r) of
    (0, True) -> error "failed to backtrack"
    (0, _   ) -> do
      tiles %= M.insert newPoint False
      stack %= drop 1
    (_, True) -> robot .= p
    (x, _)    -> do
      when (x == 2) $ goal .= newPoint
      robot .= newPoint
      tiles %= M.insert newPoint True
      stack %= ([(newPoint, dv) | dv <- [N,E,S,W]] ++)

runMazeMonad :: MazeMonad a -> IO Maze
runMazeMonad = flip execStateT $ Maze t s o o
  where t = M.singleton o True
        s = [(o, d) | d <- [N,E,S,W]]
        o = Point 0 0



-- filling with oxygen

type FillState = (S.Set Point, Int, [Point])

findFurthestPoint :: SparseMap Tile -> Point -> Int
findFurthestPoint t sp = runFP (S.singleton sp) 0 [sp]
  where runFP !s !i !ps =
          let nps = ps >>= nextPoints t s
              ns  = S.union s $ S.fromList nps
          in if null nps
             then i
             else runFP ns (i+1) nps

nextPoints :: SparseMap Tile -> S.Set Point -> Point -> [Point]
nextPoints t s p = [ q
                   | q <- fourNeighbouringPointsOf t p
                   , t M.!? q == Just True
                   , S.notMember q s
                   ]

fillStep :: SparseMap Tile -> FillState -> FillState
fillStep t (!s, !i, !ps) = (ns, i+1, nps)
  where nps = ps >>= nextPoints t s
        ns  = S.union s $ S.fromList nps



-- pathfinding animation

step :: PFState Point -> Maybe (Logs, PFState Point)
step s
  | pfFinished s = Nothing
  | otherwise    = let n = pfStep s in Just ([], n)

render :: SparseMap Tile -> PFState Point -> String
render m s = displayWith r m
  where r _ (Just False) = fgColor white $ bgColor gray "▇▇"
        r _ Nothing      = fgColor white $ bgColor gray "▇▇"
        r p _
          | Just c <- pfCost s p = b p $ fgColor (f c) "▒▒"
          | p == pfEnd s         = b p $ fgColor (f maxCost) "▒▒"
          | otherwise            = "  "
        f c = interpolateN distanceMap $ c % maxCost
        b p
          | pfFinished s = q $ p `elem` map fst (pfPath s)
          | otherwise    = q $ p `elem` [ pfStart   s
                                        , pfEnd     s
                                        , pfCurrent s
                                        ]
        q = bgColor . bool (interpolate (5%6) white black) white
        maxCost = max 1 $ maximum $ map snd $ M.elems $ pfMap s
        gray = interpolate (1%3) white black
        distanceMap = [ interpolate (3%4) black red
                      , interpolate (3%4) black yellow
                      , interpolate (3%4) black green
                      , interpolate (3%4) black blue
                      ]

neighbours :: SparseMap Tile -> Point -> [(Int, Point)]
neighbours t = map (1,) . filter isNotAWall . fourNeighbouringPointsOf t
  where isNotAWall p = t M.!? p == Just True

heuristic :: Point -> Point -> Int
heuristic g p = manhattanNorm $ g - p

initialState :: SparseMap Tile -> Point -> Point -> PFState Point
initialState t s g = mkPFState n h s g
  where n = neighbours t
        h = heuristic  g

runPFAnimation :: SparseMap Tile -> Point -> Point -> IO ()
runPFAnimation t s g = do
  hSetEcho stdout False
  clearAndReset
  animate 20 resetCursor (render t) step ([], initialState t s g)



-- flooding animation

animFillStep :: SparseMap Tile -> FillState -> Maybe (Logs, FillState)
animFillStep t s@(_, _, ps)
  | null ps   = Nothing
  | otherwise = Just ([], fillStep t s)

renderFill :: SparseMap Tile -> FillState -> String
renderFill m (s, _, ps) = displayWith r m
  where r _ (Just False) = fgColor white $ bgColor gray "▇▇"
        r _ Nothing      = fgColor white $ bgColor gray "▇▇"
        r p _
          | p `elem`     ps = bgColor currentPointsColor "  "
          | p `S.member`  s = bgColor oldPointsColor     "  "
          | otherwise       = "  "
        gray = interpolate (1%3) white black
        currentPointsColor = green
        oldPointsColor = interpolate (1%2) black green

runFillAnimation :: SparseMap Tile -> Point -> IO ()
runFillAnimation t p = do
  hSetEcho stdout False
  clearAndReset
  animate 20 resetCursor (renderFill t) (animFillStep t) ([], (S.singleton p, 0, [p]))



-- main

main :: IO ()
main = aocMain 15 $ \rawInput -> do
  let program = parseInput rawInput
  m <- runMazeMonad $ runM program inF outF
  let t = m ^. tiles
      g = m ^. goal
      o = Point 0 0

  -- runPFAnimation t o g
  -- runFillAnimation t g
  print $ snd $ last $ findPathH (neighbours t) (heuristic g) o g
  print $ findFurthestPoint t g

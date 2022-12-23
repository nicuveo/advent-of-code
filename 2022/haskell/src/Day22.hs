module Main where


-- import

import Control.Lens         hiding (from)
import Control.Monad.Extra  (whenM)
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.State
import Data.Hashable
import Data.HashMap.Strict  (HashMap, (!))
import Data.HashMap.Strict  qualified as M
import Data.List            qualified as L
import Data.Maybe
import Data.Set             (Set)
import Data.Set             qualified as S
import Data.Traversable
import Text.Parsec
import Text.Parsec.Char

import AOC

import Debug.Trace
import Text.Printf


-- input

type Input = (World, Path, Point)

data World = World
  { wMap   :: HashMap Point (Maybe Int)
  , wEdges :: HashMap (Int, Direction) (Int, Direction)
  , wSize  :: Int
  }

data Step
  = Move Int
  | TurnLeft
  | TurnRight
  deriving (Show)

type Path  = [Step]

parseInput :: Int -> String -> Input
parseInput size input =
  ( World
    { wMap   = M.fromList worldPoints
    , wEdges = findDiceSides groups
    , wSize  = size
    }
  , steps
  , fst $ head worldPoints
  )
  where
    (world, concat -> path) = span (not . null) $ lines input
    worldPoints = do
      (y,r) <- zip [0..] world
      (x,c) <- zip [0..] r
      let p = Point x y
          g = findGroup size p
      case c of
        '.' -> pure (p, Just g)
        '#' -> pure (p, Nothing)
        ' ' -> fail "ignoring empty space"
        _   -> error "wrong input"
    groups = S.fromList $ mapMaybe snd worldPoints
    steps  = flip parseWith path $ many1 $ moveN <|> turnL <|> turnR
    moveN  = Move      <$> number
    turnL  = TurnLeft  <$  symbol "L"
    turnR  = TurnRight <$  symbol "R"

findGroup :: Int -> Point -> Int
findGroup s (Point x y) = div y s * 10 + div x s


-- dice

data Dice
  = DLeft
  | DRight
  | DTop
  | DBottom
  | DFront
  | DBack
  deriving (Bounded, Enum, Eq, Ord, Show)

instance Hashable Dice where
  hashWithSalt x = hashWithSalt x . fromEnum

findDiceSides :: Set Int -> HashMap (Int, Direction) (Int, Direction)
findDiceSides knownGroups =
  postProcess $ flip execState mempty $ go (S.findMin knownGroups) DBack N DTop
  where
    go group from edge side = do
      seen <- isJust <$> gets (M.lookup group)
      unless seen do
        -- for each side we haven't encountered yet
        -- get all of the edges, and create a temporary hashmap
        -- for each group, what side it is, and which side is in which direction
        let edges = getEdges from edge side
        modify $ M.insert group (side, edges)
        -- then visit all directly adjacent neighbours
        sequence_ do
          (neighbourGroup, goingTo, arrivingFrom) <- zip3
            [group - 1,  group - 10, group + 1,  group + 10]
            [        W,           N,         E,           S]
            [        E,           S,         W,           N]
          guard $ neighbourGroup `S.member` knownGroups
          pure $ go neighbourGroup side arrivingFrom (edges ! goingTo)
    -- post-processing finishes the job, by associating group number to group
    -- number in each direction
    postProcess mappings = M.fromList do
      -- for each group and its edges
      (groupF, (sideF, edgesF)) <- M.toList mappings
      -- for each edge
      (dirT, sT) <- M.toList edgesF
      -- for each six group
      (groupT, (sideT, edgesT)) <- M.toList mappings
      -- keep the one group that corresponds to our target side
      guard $ sT == sideT
      -- find the correct edge
      (dirF, sF) <- M.toList edgesT
      guard $ sF == sideF
      pure ((groupF, dirT), (groupT, dirF))
    getNeighbours = \case
      DLeft   -> [DTop, DFront, DBottom, DBack]
      DRight  -> [DTop, DBack, DBottom, DFront]
      DFront  -> [DTop, DRight, DBottom, DLeft]
      DBack   -> [DTop, DLeft, DBottom, DRight]
      DTop    -> [DBack, DRight, DFront, DLeft]
      DBottom -> [DFront, DRight, DBack, DLeft]
    getEdges f e s =
      let nbs = L.dropWhile (/=f) $ cycle (getNeighbours s)
          ds  = L.dropWhile (/=e) $ cycle [N,E,S,W]
       in M.fromList $ take 4 $ zip ds nbs


-- world

type Player = (Point, Direction)

type BoardMonad m =
  ( MonadReader World  m
  , MonadState  Player m
  )

run :: World -> Player -> StateT Player (Reader World) a -> a
run world player = flip runReader world . flip evalStateT player

step :: BoardMonad m => m Bool -> Step -> m ()
step move = \case
  TurnLeft  -> modify $ over _2 turn90L
  TurnRight -> modify $ over _2 turn90R
  Move n    -> n & fix \g x -> whenM move $ when (x>1) $ g (x-1)

debug :: BoardMonad m => m ()
debug = do
  World {..}       <- ask
  (pos, direction) <- get
  let group = findGroup wSize pos
  traceM $ printf "x: %2d, y: %2d, d: %s, g: %2d" (px pos) (py pos) (show direction) group


-- movement

move1 :: BoardMonad m => m Bool
move1 = do
  World {..}       <- ask
  (pos, direction) <- get
  let vector = directionVector direction
      (newPos, reachable) = head do
        n <- [1..]
        let p = Point
              (mod (px pos + n * px vector) (6 * wSize))
              (mod (py pos + n * py vector) (6 * wSize))
        Just s <- pure $ M.lookup p wMap
        pure (p, isJust s)
  when reachable $ put (newPos, direction)
  pure reachable

move2 :: BoardMonad m => m Bool
move2 = do
  World {..}       <- ask
  (pos, direction) <- get
  let vector    = directionVector direction
      candidate = pos + vector
      oldGroup  = findGroup wSize pos
      newGroup  = findGroup wSize candidate
  (newPos, newDir) <-
    if newGroup == oldGroup
    then pure (candidate, direction)
    else transpose oldGroup pos direction
  let reachable = isJust $ wMap ! newPos
  when reachable $ put (newPos, newDir)
  pure reachable

transpose :: BoardMonad m => Int -> Point -> Direction -> m (Point, Direction)
transpose group point dir = do
  World {..} <- ask
  let (targetGroup, targetDir) = wEdges ! (group, dir)
      delta = manhattanNorm $ case dir of
        N -> point - tl wSize group
        E -> point - tr wSize group
        S -> dr wSize group - point
        W -> dl wSize group - point
        _ -> error "no diagonals"
  pure $ case targetDir of
           N -> (tr wSize targetGroup - Point delta 0, S)
           E -> (dr wSize targetGroup - Point 0 delta, W)
           S -> (dl wSize targetGroup + Point delta 0, N)
           W -> (tl wSize targetGroup + Point 0 delta, E)
           _ -> error "no diagonals"
  where
    tl s (flip divMod 10 -> (r, c)) = Point (c*s)     (r*s)
    tr s (flip divMod 10 -> (r, c)) = Point (c*s+s-1) (r*s)
    dl s (flip divMod 10 -> (r, c)) = Point (c*s)     (r*s+s-1)
    dr s (flip divMod 10 -> (r, c)) = Point (c*s+s-1) (r*s+s-1)


-- solution

password :: BoardMonad m => m Int
password = do
  (Point x y, d) <- get
  let f = case d of
            E -> 0
            S -> 1
            W -> 2
            N -> 3
            _ -> error "facing diagonally?!"
  pure $ (y+1) * 1000 + (x+1) * 4 + f

part1 :: Input -> Int
part1 (world, steps, start) = run world (start, E) $
  traverse (step move1) steps >> password

part2 :: Input -> Int
part2 (world, steps, start) = run world (start, E) $
  traverse (step move2) steps >> password


-- main

main :: IO ()
main = aocMain 22 $ \rawData -> do
  let testInput = parseInput  4 example
      realInput = parseInput 50 rawData
  print $ wEdges $ view _1 testInput
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "        ...#\n        .#..\n        #...\n        ....\n...#.......#\n........#...\n..#....#....\n..........#.\n        ...#....\n        .....#..\n        .#......\n        ......#.\n\n10R5L5R10L4R5L5"

module Day17 where


-- import

import AOC
import AOC.Debug
import AOC.Grid.Flat
import "this" Prelude

import Control.Lens
import Data.Char           (digitToInt)
import Data.Hashable
import Data.HashMap.Strict qualified as M
import Data.PQueue.Min     qualified as Q
import Data.Ratio

import Data.Map            qualified as RealMap


-- input

type Input = Grid Int

parseInput :: String -> Input
parseInput = fmap digitToInt . from2DList . lines


-- solution

part1 :: Input -> Int
part1 grid = fst $ last $ unsafeFindAnyPathWith step heuristic start end
  where
    endPoint = Point (gWidth grid - 1) (gHeight grid - 1)
    start = (Point 0 0, S, 0 :: Int)
    end (p, _, _) = p == endPoint
    heuristic (p, _, _) = manhattanNorm $ endPoint - p
    step (p, d, n) = do
      (d', n') <- candidates d n
      let p' = p + directionVector d'
      guard $ inBounds grid p'
      pure (grid ! p', (p', d', n'))
    candidates d n = map fst $ filter snd $
      [ ((        d, n + 1), n < 3)
      , ((turn90L d,     1),  True)
      , ((turn90R d,     1),  True)
      ]

findSmallestHeat :: Input -> Point -> Point -> Int
findSmallestHeat grid start end = go (M.singleton (start, S, 0) 0) [((start, S, 0), 0)]
  where
    go :: HashMap (Point, Direction, Int) Int -> [((Point, Direction, Int), Int)] -> Int
    go seen [] = minimum do
      ((p, _, _), v) <- M.toList seen
      guard $ p == end
      return v
    go seen current = uncurry go $ foldl' visit (seen, []) current

    visit
      :: (HashMap (Point, Direction, Int) Int, [((Point, Direction, Int), Int)])
      -> ((Point, Direction, Int), Int)
      -> (HashMap (Point, Direction, Int) Int, [((Point, Direction, Int), Int)])
    visit (seen, newPoints) c =
      let c' = step seen c
      in  (M.unionWith min seen $ M.fromList c', newPoints ++ c')

    step
      :: HashMap (Point, Direction, Int) Int
      -> ((Point, Direction, Int), Int)
      -> [((Point, Direction, Int), Int)]
    step seen ((p, d, n), h) = do
      (d', n') <- candidates d n
      let p' = p + directionVector d'
          h' = h + grid ! p'
      guard $ h < 1100
      guard $ inBounds grid p'
      guard $ case M.lookup (p', d', n') seen of
        Nothing           -> True
        Just previousCost -> previousCost > h'
      pure ((p', d', n'), h')

    candidates :: Direction -> Int -> [(Direction, Int)]
    candidates d n = map fst $ filter snd $
      [ ((        d, n + 1), n < 10)
      , ((turn90L d,     1), n == 0 || n >= 4)
      , ((turn90R d,     1), n == 0 || n >= 4)
      ]

{-
part2 :: Input -> Int
part2 grid = findSmallestHeat grid startPoint endPoint
  where
    endPoint = Point (gWidth grid - 1) (gHeight grid - 1)
    startPoint = Point 0 0
-}

part2 :: Input -> Int
part2 grid = fst $ last $ unsafeFindAnyPathWith step heuristic start end
  where
    endPoint = Point (gWidth grid - 1) (gHeight grid - 1)
    start = (Point 0 0, E, 0 :: Int)
    end (p, _, n) = p == endPoint && n >= 4
    heuristic (p, _, _) = 0 -- manhattanNorm $ endPoint - p
    step (p, d, n) = do
      (d', n') <- candidates d n
      let p' = p + directionVector d'
      guard $ inBounds grid p'
      pure (grid ! p', (p', d', n'))
    candidates d n = map fst $ filter snd $
      [ ((        d, n + 1), n < 10)
      , ((turn90L d,     1), n == 0 || n >= 4)
      , ((turn90R d,     1), n == 0 || n >= 4)
      ]
    {-
    step (p, d, n) = catMaybes [goStraight p d n, turnRight p d, turnLeft p d]
    goStraight p d n = do
      guard $ n < 10
      let p' = p + directionVector d
      guard $ inBounds grid p'
      pure (grid ! p', (p', d, n+1))
    turnLeft  = turnWith turn90L
    turnRight = turnWith turn90R
    turnWith f p d = do
      let d' = f d
          v  = directionVector d'
          ps = [p + n .* v | n <- [1..4]]
      guard $ inBounds grid (last ps)
      pure (sum $ map (grid !) ps, (last ps, d', 4))
    -}


-- main

example :: String
example = "2413432311323\n3215453535623\n3255245654254\n3446585845452\n4546657867536\n1438598798454\n4457876987766\n3637877979653\n4654967986887\n4564679986453\n1224686865563\n2546548887735\n4322674655533"

{-pfRender m s = unlines do
  let
    w = gWidth  m
    h = gHeight m
  y <- [h-20..h-1]
  pure do
    x <- [w-50..w-1]
    r (Point x y) $ m ! (Point x y)
  where
    r p n
      | isCurrent   p = fgColor red   $ show n
      | isInQueue   p = fgColor green $ show n
      | hasBeenSeen p = fgColor cyan  $ show n
      | otherwise     = show n
    isCurrent p = case Q.minView (pfQueue s) of
      Nothing                            -> False
      Just (cellNode . fst -> (q, _, _)) -> p == q
    isInQueue p = p `elem` [q | (q, _, _) <- map cellNode $ Q.toList (pfQueue s)]
    hasBeenSeen p = p `elem` [q | (q, _, _) <- M.keys (pfNodeInfo s)]
-}

pfStep s
  | hasFoundAnswer s = Nothing
  | otherwise        = let n = pathFindingStep s in Just ([], n)

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  -- putStrLn "# Part 1"
  -- print $ part1 testInput
  -- print $ part1 realInput
  -- putStrLn "# Part 2"
  -- print $ part2 testInput
  -- print $ part2 realInput
  -- let
  --   endPoint = Point (gWidth realInput - 1) (gHeight realInput - 1)
  --   start = (Point 0 0, S, 0 :: Int)
  --   end (p, _, n) = p == endPoint && n >= (4 :: Int)
  --   heuristic (p, _, _) = manhattanNorm $ endPoint - p
  --   step (p, d, n) = do
  --     (d', n') <- candidates d n
  --     let p' = p + directionVector d'
  --     guard $ inBounds realInput p'
  --     pure (realInput ! p', (p', d', n'))
  --   candidates d n = map fst $ filter snd $
  --     [ ((        d, n + 1), n < 10)
  --     , ((turn90L d,     1), n == 0 || n >= 4)
  --     , ((turn90R d,     1), n == 0 || n >= 4)
  --     ]
  --   initialState = mkPFState step heuristic (Point 0 0, S, 0) end
  -- animate 20 resetCursor (pfRender realInput) pfStep ([], initialState)
  let m = RealMap.empty
  void $ flip execStateT m $
    for_ [0..140] \x ->
      for_ [0..140] \y ->
        for_ [minBound :: Direction, maxBound :: Direction] \d ->
          for_ [0..10] \(n :: Int) -> do
            cache <- get
            let v = (Point x y, d, n)
                h = hash v
            case RealMap.lookup h cache of
              Nothing -> do
                modify $ RealMap.insert h [v]
              Just x  -> do
                let nx = v:x
                liftIO $ putStrLn $ show h ++ " => " ++ intercalate ", " (map show nx)
                modify $ RealMap.insert h nx

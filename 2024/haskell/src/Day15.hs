{-# LANGUAGE TemplateHaskell #-}

module Day15 where


-- import

import AOC
import AOC.Grid.Flat
import "this" Prelude

import Control.Lens
import Control.Monad.Extra (whenM)
import Data.Maybe          (fromJust)
import Data.Monoid         (First (..))
import Text.Parsec


-- input

data Entity
  = Wall
  | Box
  deriving (Show, Eq)

data LargeEntity
  = LargeWall
  | LeftBox
  | RightBox
  deriving (Show, Eq)

type Input = (Grid (Maybe Entity), Point, [Direction])

parseInput :: String -> Input
parseInput = parseWith do
  rawLines <- many1 gridLine
  let width  = length $ head rawLines
      height = length rawLines
      (entities, startingPositions) = postProcess rawLines
  instructions <- many1 direction
  pure ( fromList width height entities
       , fromJust $ getFirst $ mconcat startingPositions
       , instructions
       )
  where
    gridLine = many1 entity <* many newline
    entity = tryAll
      [ (Just Wall, Nothing) <$ char '#'
      , (Just Box,  Nothing) <$ char 'O'
      , (Nothing,   Just ()) <$ char '@'
      , (Nothing,   Nothing) <$ char '.'
      ]
    postProcess rawLines = unzip do
      (y, line) <- zip [0..] rawLines
      (x, (e, startingPosition)) <- zip [0..] line
      pure (e, First $ Point x y <$ startingPosition)
    direction = tryAll
      [ N <$ symbol "^"
      , W <$ symbol "<"
      , E <$ symbol ">"
      , S <$ symbol "v"
      ]


-- part 1

data Warehouse e = Warehouse
  { _layout :: Grid (Maybe e)
  , _robot  :: Point
  }

makeLenses ''Warehouse

step1
  :: MonadState (Warehouse Entity) m
  => Direction
  -> m ()
step1 dir = do
  currentPosition <- use robot
  let vector = directionVector dir
      candidatePosition = currentPosition + vector
  whenM (canMoveTo1 candidatePosition vector) do
    robot .= candidatePosition

canMoveTo1
  :: MonadState (Warehouse Entity) m
  => Point
  -> Vector
  -> m Bool
canMoveTo1 point vector =
  uses layout (! point) >>= \case
    Nothing   -> pure True
    Just Wall -> pure False
    Just Box  -> do
      boxCanBePushed <- canMoveTo1 (point + vector) vector
      when boxCanBePushed do
        layout %= updateAt (point + vector) (Just Box)
        layout %= updateAt point Nothing
      pure boxCanBePushed

part1 :: Input -> Int
part1 (grid, startingPosition, instructions) =
  let Warehouse endLayout _ =
        flip execState (Warehouse grid startingPosition) $
          traverse_ step1 instructions
  in sum do
       p@(Point x y) <- allPoints endLayout
       pure $ case endLayout ! p of
         Just Box -> y * 100 + x
         _        -> 0


-- part 2

widen :: Grid (Maybe Entity) -> Grid (Maybe LargeEntity)
widen oldGrid = fromList (gWidth oldGrid * 2) (gHeight oldGrid) do
  y <- yRange oldGrid
  x <- xRange oldGrid
  let p = Point x y
  case oldGrid ! p of
    Nothing   -> [Nothing, Nothing]
    Just Wall -> [Just LargeWall, Just LargeWall]
    Just Box  -> [Just LeftBox,   Just RightBox]

step2
  :: MonadState (Warehouse LargeEntity) m
  => Direction
  -> m ()
step2 dir = do
  currentPosition <- use robot
  let vector = directionVector dir
      candidatePosition = currentPosition + vector
  beforePushing <- get
  robotCanMove  <- canMoveTo2 candidatePosition dir vector
  if robotCanMove
  then do
    robot .= candidatePosition
  else do
    put beforePushing

canMoveTo2
  :: MonadState (Warehouse LargeEntity) m
  => Point
  -> Direction
  -> Vector
  -> m Bool
canMoveTo2 point dir vector =
  uses layout (! point) >>= \case
    Nothing        -> pure True
    Just LargeWall -> pure False
    Just box       ->
      if dir == W || dir == E
      then do
        boxCanBePushed <- canMoveTo2 (point + vector) dir vector
        when boxCanBePushed do
          layout %= updateAt (point + vector) (Just box)
          layout %= updateAt point Nothing
        pure boxCanBePushed
      else do
        let (pointL, pointR) = boxPoints box
        boxLCanBePushed <- canMoveTo2 (pointL + vector) dir vector
        boxRCanBePushed <- canMoveTo2 (pointR + vector) dir vector
        when (boxLCanBePushed && boxRCanBePushed) do
          layout %= updateAt (pointL + vector) (Just LeftBox)
          layout %= updateAt (pointR + vector) (Just RightBox)
          layout %= updateAt pointL Nothing
          layout %= updateAt pointR Nothing
        pure $ boxLCanBePushed && boxRCanBePushed
  where
    boxPoints = \case
      LeftBox  -> (point, rightOf point)
      RightBox -> (leftOf point,  point)
      _        -> error "tried to push a wall!"

part2 :: Input -> Int
part2 (grid, (Point startX startY), instructions) =
  let Warehouse endLayout _ =
        flip execState (Warehouse (widen grid) (Point (2*startX) startY)) $
          traverse_ step2 instructions
  in sum do
       p@(Point x y) <- allPoints endLayout
       pure $ case endLayout ! p of
         Just LeftBox -> y * 100 + x
         _            -> 0


-- main

example1 :: String
example1 = "\
\########\n\
\#..O.O.#\n\
\##@.O..#\n\
\#...O..#\n\
\#.#.O..#\n\
\#...O..#\n\
\#......#\n\
\########\n\
\\n\
\<^^>>>vv<v>>v<<"

example2 :: String
example2 = "\
\##########\n\
\#..O..O.O#\n\
\#......O.#\n\
\#.OO..O.O#\n\
\#..O@..O.#\n\
\#O#..O...#\n\
\#O..O..O.#\n\
\#.OO.O.OO#\n\
\#....O...#\n\
\##########\n\
\\n\
\<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\n\
\vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n\
\><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n\
\<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n\
\^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n\
\^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n\
\>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n\
\<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n\
\^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\n\
\v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

render :: Point -> Point -> Maybe LargeEntity -> String
render startPoint currentPoint maybeEntity
  | startPoint == currentPoint = "@"
  | otherwise = case maybeEntity of
      Nothing        -> " "
      Just LargeWall -> "#"
      Just LeftBox   -> "["
      Just RightBox  -> "]"

main :: String -> IO ()
main rawData = do
  let testInput1 = parseInput example1
      testInput2 = parseInput example2
      realInput  = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput1
  print $ part1 testInput2
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput2
  print $ part2 realInput

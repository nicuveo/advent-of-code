-- import

import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Char
import           Data.Foldable               (for_)
import           Data.Function               (on)
import           Data.List
import           Data.List.Split             (chunksOf)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as V

import           AOC.Debug.Animate           (animateM, clearAndReset)
import           AOC.Debug.Color
import           AOC.Runtime


-- input

type Input = V.Vector Int

parseInput :: String -> Input
parseInput = V.fromList . map digitToInt . filter isDigit


-- point stuff

type Point = (Int, Int)

inBounds :: Point -> Bool
inBounds (x,y) = x >= 0 && y >= 0 && x < 10 && y < 10

neighbours :: Point -> [Point]
neighbours (x,y) = filter inBounds
  [ (x-1, y-1), (x-1, y), (x-1, y+1)
  , (x,   y-1)          , (x,   y+1)
  , (x+1, y-1), (x+1, y), (x+1, y+1)
  ]

forAll :: Monad m => (Point -> m ()) -> m ()
forAll f =
  for_ [0..9] $ \y ->
    for_ [0..9] $ \x ->
      f (x,y)


-- mutable map

type MutableMap s = V.MVector s Int
type ExecutionMonad m = StateT Int (ReaderT (MutableMap (PrimState m)) m)

readOctopus :: PrimMonad m => Point -> ExecutionMonad m Int
readOctopus p@(x,y) = if inBounds p
  then do
    octoMap <- ask
    V.read octoMap $ y*10+x
  else
    error $ "tried to read out of bounds: " ++ show p

writeOctopus :: PrimMonad m => Point -> Int -> ExecutionMonad m ()
writeOctopus p@(x,y) v = if inBounds p
  then do
    octoMap <- ask
    V.write octoMap (y*10+x) v
  else
    error $ "tried to read out of bounds: " ++ show p

plusOne :: PrimMonad m => Point -> ExecutionMonad m ()
plusOne p = do
  v <- readOctopus p
  writeOctopus p (v+1)

zero :: PrimMonad m => Point -> ExecutionMonad m ()
zero p = writeOctopus p 0


-- solution

step :: PrimMonad m => ExecutionMonad m ()
step = do
  forAll plusOne
  forAll flash
  where
    flash p = do
      v <- readOctopus p
      when (v > 9) $ do
        zero p
        modify (+1)
        for_ (neighbours p) $ \neighbour -> do
          neighbourValue <- readOctopus neighbour
          when (neighbourValue > 0) $ do
            plusOne neighbour
            flash neighbour

runFor :: Int -> Input -> Int
runFor iterations start = runST $ do
  octoMap <- V.thaw start
  flip runReaderT octoMap $
    flip execStateT 0 $
      for_ [1..iterations] $
        const step

runUntilAllFlash :: Input -> Int
runUntilAllFlash start = runST $ do
  octoMap <- V.thaw start
  flip runReaderT octoMap $
    flip evalStateT 0 $
      go 0 0
  where
    go stepN previousFlashes = do
      step
      currentFlashes <- get
      if currentFlashes - previousFlashes == 100
      then pure $ stepN+1
      else go (stepN + 1) currentFlashes

part1 :: Input -> Int
part1 = runFor 100

part2 :: Input -> Int
part2 = runUntilAllFlash


-- rendering

render :: V.Vector Int -> String
render v = unlines $ map (concatMap renderValue) $ chunksOf 10 $ V.toList v
  where
    renderValue :: Int -> String
    renderValue 0 = fgColor cyan "0"
    renderValue x = show x

update :: V.Vector Int -> ExecutionMonad IO (Maybe ([String], V.Vector Int))
update _ = do
  step
  mutableMap <- ask
  octoMap <- V.freeze mutableMap
  pure $ Just ([], octoMap)

animate :: Input -> IO ()
animate start = do
  octoMap <- V.thaw start
  flip runReaderT octoMap $
    flip evalStateT 0 $
      animateM 1000 clearAndReset render update $ pure ([], start)


-- main

main :: IO ()
main = aocMain 11 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput
  -- animate testInput

example :: String
example = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"

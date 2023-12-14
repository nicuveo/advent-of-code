{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Day14 where


-- import

import AOC                         hiding (symbol)
import "this" Prelude

import Control.Lens                hiding (Empty, (|>))
import Control.Monad.ST
import Data.Hashable
import Data.HashMap.Strict         qualified as M
import Data.List                   (transpose)
import Data.List.Split
import Data.Sequence               (Seq (..), (|>))
import Data.Vector.Unboxed         qualified as V
import Data.Vector.Unboxed.Mutable (PrimMonad, PrimState)
import Data.Vector.Unboxed.Mutable qualified as MV


-- input

type Input = [String]

parseInput :: String -> Input
parseInput = lines


-- part 1

part1 :: Input -> Int
part1 = sum . concatMap (computeWeight . reverse) . transpose
  where
    computeWeight = map segmentWeight . wordsBy ((== '#') . snd) . zip [1..]
    segmentWeight (unzip -> (weights, segment)) =
      sum $ take (count 'O' segment) $ reverse weights


-- part 2

data PlatformInfo m = PlatformInfo
  { platformData   :: V.MVector (PrimState m) Char
  , platformWidth  :: Int
  , platformHeight :: Int
  }

data PlatformState = PlatformState
  { _freeSpaces     :: Seq Int
  , _iterationCount :: Int
  , _currentWeight  :: Int
  , _stateCache     :: HashMap Int Int
  } deriving Show

makeLenses ''PlatformState


newtype PlatformM m a = PlatformM
  { runPlatform :: ReaderT (PlatformInfo m) (StateT PlatformState m) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (PlatformInfo m)
    , MonadState PlatformState
    , PrimMonad
    , MonadIO
    )

run :: PrimMonad m => Input -> PlatformM m a -> m a
run input computation = do
  platformInfo <- mkPlatformInfo
  evalStateT (runReaderT (runPlatform computation) platformInfo) initialState
  where
    initialState = PlatformState Empty 0 cw mempty
    mkPlatformInfo = do
      let height = length input
          width  = length $ head input
      vec <- V.thaw $ V.fromList $ concat input
      pure $ PlatformInfo vec width height
    cw = sum do
      column <- transpose input
      (weight, symbol) <- zip [1..] $ reverse column
      guard $ symbol == 'O'
      pure weight

runWithST :: forall a. Input -> (forall s. PlatformM (ST s) a) -> a
runWithST input computation = runST $ run input computation

runWithIO :: Input -> PlatformM IO a -> IO a
runWithIO input computation = run input computation


inspect :: PrimMonad m => Point -> PlatformM m Char
inspect (Point x y) = do
  PlatformInfo {..} <- ask
  let i = y * platformWidth + x
  MV.read platformData i

moveRockFromTo :: PrimMonad m => Point -> Point -> PlatformM m ()
moveRockFromTo (Point fx fy) (Point tx ty) = do
  PlatformInfo {..} <- ask
  let indexF = fy * platformWidth + fx
      indexT = ty * platformWidth + tx
  MV.write platformData indexF '.'
  MV.write platformData indexT 'O'
  currentWeight += fy - ty


tiltN :: PrimMonad m => PlatformM m ()
tiltN = do
  PlatformInfo {..} <- ask
  for_ [0 .. platformWidth-1] \x -> do
    freeSpaces .= Empty
    for_ [0 .. platformHeight-1] \y -> do
      inspect (Point x y) >>= \case
        '.' -> freeSpaces %= (|> y)
        '#' -> freeSpaces .= Empty
        'O' -> use freeSpaces >>= \case
          Empty        -> pure ()
          (fs :<| fss) -> do
            moveRockFromTo (Point x y) (Point x fs)
            freeSpaces .= fss |> y
        c -> error $ "unexpected character: " ++ [c]

tiltS :: PrimMonad m => PlatformM m ()
tiltS = do
  PlatformInfo {..} <- ask
  for_ [0 .. platformWidth-1] \x -> do
    freeSpaces .= Empty
    for_ [platformHeight-1, platformHeight-2 .. 0] \y -> do
      inspect (Point x y) >>= \case
        '.' -> freeSpaces %= (|> y)
        '#' -> freeSpaces .= Empty
        'O' -> use freeSpaces >>= \case
          Empty        -> pure ()
          (fs :<| fss) -> do
            moveRockFromTo (Point x y) (Point x fs)
            freeSpaces .= fss |> y
        c -> error $ "unexpected character: " ++ [c]

tiltW :: PrimMonad m => PlatformM m ()
tiltW = do
  PlatformInfo {..} <- ask
  for_ [0 .. platformHeight-1] \y -> do
    freeSpaces .= Empty
    for_ [0 .. platformWidth-1] \x -> do
      inspect (Point x y) >>= \case
        '.' -> freeSpaces %= (|> x)
        '#' -> freeSpaces .= Empty
        'O' -> use freeSpaces >>= \case
          Empty        -> pure ()
          (fs :<| fss) -> do
            moveRockFromTo (Point x y) (Point fs y)
            freeSpaces .= fss |> x
        c -> error $ "unexpected character: " ++ [c]

tiltE :: PrimMonad m => PlatformM m ()
tiltE = do
  PlatformInfo {..} <- ask
  for_ [0 .. platformHeight-1] \y -> do
    freeSpaces .= Empty
    for_ [platformWidth-1, platformWidth-2 .. 0] \x -> do
      inspect (Point x y) >>= \case
        '.' -> freeSpaces %= (|> x)
        '#' -> freeSpaces .= Empty
        'O' -> use freeSpaces >>= \case
          Empty        -> pure ()
          (fs :<| fss) -> do
            moveRockFromTo (Point x y) (Point fs y)
            freeSpaces .= fss |> x
        c -> error $ "unexpected character: " ++ [c]

getHash :: PrimMonad m => PlatformM m Int
getHash = do
  PlatformInfo {..} <- ask
  MV.foldl' hashWithSalt (hash ' ') platformData

tiltCycle :: PrimMonad m => PlatformM m ()
tiltCycle = do
  tiltN
  tiltW
  tiltS
  tiltE
  iterationCount += 1

findCycle :: PrimMonad m => PlatformM m Int
findCycle = do
  tiltCycle
  stateHash <- getHash
  currentCycle <- use iterationCount
  uses stateCache (M.lookup stateHash) >>= \case
    Just c  -> pure c
    Nothing -> do
      stateCache %= M.insert stateHash currentCycle
      findCycle


display :: PlatformM IO ()
display = do
  PlatformInfo {..} <- ask
  for_ [0 .. platformHeight-1] \y -> do
    for_ [0 .. platformWidth-1] \x -> do
      c <- inspect (Point x y)
      liftIO $ putStr [c]
    liftIO $ putStrLn ""

part2 :: Input -> Int
part2 input = runWithST input do
  start <- findCycle
  end   <- use iterationCount
  let leftToDo = mod (1000000000 - end) (end - start)
  replicateM_ leftToDo tiltCycle
  use currentWeight


-- main

example :: String
example = "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

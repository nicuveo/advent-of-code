{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}



-- import

import           Control.Monad.Base
import           Control.Monad.Extra
import           Control.Monad.Reader
import           Control.Monad.ST
import           Data.Has
import           Data.List.Split
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as V

import           AOC



-- input

type Input = [Int]

parseInput :: String -> Input
parseInput = map read . splitOn ","



-- program

newtype Program s = Program (V.MVector s Int)

type MonadProgram r s m = ( MonadReader r m
                          , Has (Program s) r
                          , MonadBase (ST s) m
                          )


get :: MonadProgram r s m => Int -> m Int
get x = do
  Program v <- asks getter
  liftBase $ V.read v x

set :: MonadProgram r s m => Int -> Int -> m ()
set i x = do
  Program v <- asks getter
  liftBase $ V.write v i x


type Instruction m = Int -> m Int

addInst :: MonadProgram r s m => Instruction m
addInst index = do
  a <- get =<< get (index + 1)
  b <- get =<< get (index + 2)
  d <- get $ index + 3
  set d $ a + b
  return $ index + 4

mulInst :: MonadProgram r s m => Instruction m
mulInst index = do
  a <- get =<< get (index + 1)
  b <- get =<< get (index + 2)
  d <- get $ index + 3
  set d $ a * b
  return $ index + 4

step :: MonadProgram r s m => Int -> m (Maybe Int)
step index = do
  opCode <- get index
  case opCode of
    1  -> Just <$> addInst index
    2  -> Just <$> mulInst index
    99 -> return Nothing
    _  -> error $ "unexpected opCode: " ++ show opCode

exec :: MonadProgram r s m => m ()
exec = exec_ 0
  where exec_ i = whenJustM (step i) exec_

run :: V.Vector Int -> V.Vector Int
run program = runST $ do
  v <- V.thaw program
  runReaderT exec $ Program v
  V.freeze v



-- solution

part1 :: Input -> Int
part1 input = run program V.! 0
  where program = V.fromList input V.// [(1, 12), (2, 2)]

part2 :: Input -> Int
part2 input = head [ 100 * n + v
                   | n <- [0..99]
                   , v <- [0..99]
                   , answer n v == 19690720
                   ]
  where answer n v = run (V.fromList input V.// [(1, n), (2, v)]) V.! 0



-- main

main :: IO ()
main = aocMain 2 $ \rawInput -> do
  let input = parseInput rawInput
  print $ part1 input
  print $ part2 input

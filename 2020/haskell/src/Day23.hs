{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

-- import

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.ST.Class
import           Data.Char                   (digitToInt, intToDigit)
import           Data.Function               (on)
import           Data.List
import           Data.List.Split             (splitOn)
import           Data.Maybe
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as V

import           AOC



-- input

type Cups = [Int]

parseInput :: String -> Cups
parseInput = map digitToInt



-- helpers (TODO: move to lib)

(^.) :: (a -> a) -> Int -> (a -> a)
(^.) _ 0  = id
(^.) f 1  = f
(^.) f !n = f . (f ^. (n-1))

(<^.>) :: Monad m => (a -> m a) -> Int -> (a ->  m a)
(<^.>) _ 0  = pure
(<^.>) f 1  = f
(<^.>) f !n = f >=> (f <^.> (n-1))



-- solution

type IndexMap s = V.MVector s Int
type MonadIndex m = (MonadST m, MonadReader (Int, IndexMap (World m)) m, MonadFail m)


getNext :: MonadIndex m => Int -> m Int
getNext cup = do
  imap <- asks snd
  liftST $ V.unsafeRead imap (cup-1)

setNext :: MonadIndex m => Int -> Int -> m ()
setNext cup nextCup = do
  imap <- asks snd
  liftST $ V.unsafeWrite imap (cup-1) nextCup

takeNext :: MonadIndex m => Int -> Int -> m [Int]
takeNext 0  _ = pure []
takeNext !n c = do
  x <- getNext c
  (x:) <$> takeNext (n-1) x


makeIndexMap :: Int -> [Int] -> ST s (IndexMap s)
makeIndexMap size prefix = do
  v <- V.new size
  void $ flip runReaderT (size,v) $ do
    sequence_ $ do
      (n, n') <- zip prefix $ tail prefix
      pure $ setNext n n'
    let m = maximum prefix + 1
    if (m > size)
    then setNext (last prefix) (head prefix)
    else do
      setNext (last prefix) m
      sequence_ $ do
        x <- [m..size-1]
        pure $ setNext x $ x + 1
      setNext size $ head prefix
  pure v

step :: MonadIndex m => Int -> m Int
step current = do
  [a,b,c,d] <- takeNext 4 current
  size      <- asks fst
  let target = head $ do
        x <- [current-1,current-2..1] ++ [size,size-1..]
        guard $ x `notElem` [a,b,c]
        pure x
  r <- getNext target
  setNext c r
  setNext target a
  setNext current d
  pure d


part1 :: Cups -> String
part1 cups = runST $ do
  let size = length cups
  v <- makeIndexMap size cups
  flip runReaderT (size, v) $ do
    void $ step <^.> 100 $ head cups
    map intToDigit <$> takeNext 8 1

part2 :: Cups -> Int
part2 cups = runST $ do
  let size = 1000000
  v <- makeIndexMap size cups
  flip runReaderT (size, v) $ do
    void $ step <^.> 10000000 $ head cups
    product <$> takeNext 2 1



-- main


main :: IO ()
main = aocMain 23 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  putStrLn $ part1 testInput
  putStrLn $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "389125467"

{-# LANGUAGE BangPatterns #-}

module AOC.Cycle where



-- imports

import           Control.Monad.Identity
import qualified Data.Map.Lazy          as M



-- functions

findLoop :: Ord a => [a] -> (Int, [a])
findLoop values = run M.empty 0 values
  where run _ _ [] = error "findLoop: data exhausted, no loop found"
        run history !n (a:l) =
          case history M.!? a of
            Just start -> (start, take (n-start) $ drop start values)
            Nothing    -> run (M.insert a n history) (n+1) l


findCycle :: Ord a => (a -> a) -> a -> (Int, [a])
findCycle f = runIdentity . findCycleM (return . f)

findCycleM :: (Monad m, Ord a) => (a -> m a) -> a -> m (Int, [a])
findCycleM stepFun = run M.empty 0
  where run history !n !a =
          case history M.!? a of
            Just start -> return (start, [])
            Nothing    -> do
              (s, l) <- run (M.insert a n history) (n+1) =<< stepFun a
              return $ if s <= n
                       then (s, a:l)
                       else (s,   l)


findFixPoint :: Ord a => (a -> a) -> a -> (Int, a)
findFixPoint f = runIdentity . findFixPointM (return . f)

findFixPointM :: (Monad m, Ord a) => (a -> m a) -> a -> m (Int, a)
findFixPointM stepFun = run 0
  where run !n !a = do
          b <- stepFun a
          if b == a
            then return (n, a)
            else run (n+1) b


iterateN :: Ord a => Int -> (a -> a) -> a -> a
iterateN target stepFun = runIdentity . iterateNM target (return . stepFun)

iterateNM :: (Monad m, Ord a) => Int -> (a -> m a) -> a -> m a
iterateNM target stepFun = fmap snd . run M.empty 0
  where run history !n !a
          | n == target = return (n, a)
          | otherwise   =
              case history M.!? a of
                Just start -> return (start + mod (target - n) (n - start), a)
                Nothing    -> do
                  (r, x) <- run (M.insert a n history) (n+1) =<< stepFun a
                  return $ if r == n then (r, a) else (r, x)

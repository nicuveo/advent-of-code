{-# LANGUAGE FlexibleContexts #-}


-- import

import           Control.Lens
import           Control.Monad.ST
import           Control.Monad.State
import           Data.List
import           Data.List.Split
import qualified Data.Vector         as BV
import qualified Data.Vector.Unboxed as V

import           AOC
import           IntCode



-- input

type Input = V.Vector Int

parseInput :: String -> Input
parseInput = V.fromList . map read . splitOn ","



-- concurrent vm io: chaining

inF :: MonadState (BV.Vector [Int]) m => Int -> m (Either ErrorCode Int)
inF p = do
  ib <- gets (^?! ix p)
  case ib of
    [] -> return $ Left WaitForInput
    (x:xs) -> do
     ix p .= xs
     return $ Right x

outF :: MonadState (BV.Vector [Int]) m => Int -> Int -> m ()
outF p x = do
  n <- gets length
  ix (mod (p+1) n) %= (++[x])

runC :: Monad m => [[Int]] -> StateT (BV.Vector [Int]) m a -> m [[Int]]
runC ibs e = BV.toList <$> execStateT e (BV.fromList ibs)



-- solution

part1 :: Input -> Int
part1 program = maximum $ map computeOutput $ permutations [0,1,2,3,4]
  where computeOutput = foldl executeOnce 0
        executeOnce signal ps = head $ snd $ run program [ps, signal]


part2 :: Input -> Int
part2 program = maximum $ map computeOutput $ permutations [5,6,7,8,9]
  where programs = replicate 5 program
        computeOutput (p:ps) = head $ concat $ runP ([p,0] : map pure ps)
        computeOutput _      = error "wat"
        runP ibs = runST $ runC ibs $ runConcurrentlyM programs inF outF



-- main

main :: IO ()
main = aocMain 7 $ \rawInput -> do
  let input = parseInput rawInput
  print $ part1 input
  print $ part2 input

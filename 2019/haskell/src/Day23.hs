{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}


-- import

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Extra
import           Control.Monad.ST
import           Control.Monad.State
import           Data.List.Split
import qualified Data.Map             as M
import qualified Data.Vector.Unboxed  as V

import           AOC
import           IntCode



-- input

type Program = V.Vector Int

parseProgram :: String -> Program
parseProgram = V.fromList . map read . splitOn ","



-- concurrent vm io: network!

type NetworkState = ( M.Map Int [Int] -- input
                    , M.Map Int [Int] -- output
                    , M.Map Int Int   -- starved
                    )

newtype ShortCircuit = PacketFound Int deriving (Show, Eq)

type NetworkMonad m = (MonadError ShortCircuit m, MonadState NetworkState m)


append = flip (++)

inF :: NetworkMonad m => Int -> m (Either ErrorCode Int)
inF p = do
  ib <- uses _1 (M.! p)
  case ib of
    []     -> do
      whenM (uses _3 $ all (>4) . M.elems) $ do
        nat <- uses _1 $ M.findWithDefault [] 255
        case nat of
          [x,y] -> do
            _3 %= M.map (const 0)
            modify $ over _1 $ M.insertWith append   0 [x,y]
            modify $ over _1 $ M.insert            255 []
            modify $ over _2 $ M.insertWith (++)   255 [y]
            ob <- uses _2 $ M.findWithDefault [] 255
            case ob of
              (y1:y2:_) -> when (y1 == y2) $ throwError $ PacketFound y2
              _         -> return ()
          _ -> return ()
      modify $ over _3 $ M.insertWith (+) p 1
      return $ Right (-1)
    (x:xs) -> do
      modify $ over _1 $ M.insert p xs
      return $ Right x

outF :: NetworkMonad m => Bool -> Int -> Int -> m ()
outF shortCircuitOnFirst p y = do
  ob <- uses _2 $ M.findWithDefault [] p
  _3 %= M.map (const 0)
  case ob of
    [255,x] -> do
      when shortCircuitOnFirst $ throwError $ PacketFound y
      modify $ over _1 $ M.insert 255 [x,y]
      modify $ over _2 $ M.insert   p []
    [d,x] -> do
      modify $ over _1 $ M.insertWith append d [x,y]
      modify $ over _2 $ M.insert            p []
    _ ->
      modify $ over _2 $ M.insertWith append p [y]

runC :: Monad m => Int -> StateT NetworkState m a -> m a
runC n = flip evalStateT ( M.fromList [(x,[x]) | x <- [0..n-1]]
                         , M.empty
                         , M.fromList [(x,0)   | x <- [0..n-1]]
                         )



-- solution

part1 :: Program -> Int
part1 program = either getPacket undefined $ runST $ runExceptT $ runC 50 $
                error "packet not found" <$ runConcurrentlyM Eager programs inF (outF True)
  where programs = replicate 50 program
        getPacket (PacketFound y) = y


part2 :: Program -> Int
part2 program = either getPacket undefined $ runST $ runExceptT $ runC 50 $
                error "packet not found" <$ runConcurrentlyM Eager programs inF (outF False)
  where programs = replicate 50 program
        getPacket (PacketFound y) = y




-- main

main :: IO ()
main = aocMain 23 $ \rawInput -> do
  let program = parseProgram rawInput
  print $ part1 program
  print $ part2 program

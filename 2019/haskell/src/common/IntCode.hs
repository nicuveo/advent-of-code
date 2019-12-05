{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE ParallelListComp #-}

module IntCode where



-- import

import           Control.Monad.Base
import           Control.Monad.Extra
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.ST
import           Data.Has
import           Data.List                   (intercalate, transpose)
import           Data.List.Split             (chunksOf)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as V
import           Text.Printf

import           AOC.Debug.Color



-- program

newtype Program s = Program (V.MVector s Int)

type MonadProgram r b m = ( PrimMonad b
                          , MonadReader r m
                          , Has (Program (PrimState b)) r
                          , MonadBase b m
                          )



-- debug

showProgram :: MonadProgram r b m => Int -> m String
showProgram index = do
  Program mv <- asks getter
  v <- liftBase $ V.toList <$> V.freeze mv
  let chunks  = chunksOf 8 v
      widths    = map (maximum . map (length . show)) $ transpose chunks
      display r c x
        | r*8 + c /= index =                make c x
        | otherwise         = bgColor blue $ make c x
      make c = printf $ "%" ++ show (widths !! c) ++ "d"
  return $ unlines [ printf "[%s]" $ intercalate ", " [ display r c x
                                                      | c <- [0..]
                                                      | x <- line
                                                      ]
                   | r    <- [0..]
                   | line <- chunks
                   ]



-- memory access

get :: MonadProgram r b m => Int -> m Int
get x = do
  Program v <- asks getter
  liftBase $ V.read v x

set :: MonadProgram r b m => Int -> Int -> m ()
set i x = do
  Program v <- asks getter
  liftBase $ V.write v i x



-- instructions

type Instruction m = Int -> m Int

binaryI :: MonadProgram r b m => (Int -> Int -> Int) -> Instruction m
binaryI (#) index = do
  a <- get =<< get (index + 1)
  b <- get =<< get (index + 2)
  d <- get $ index + 3
  set d $ a # b
  return $ index + 4

addI :: MonadProgram r b m => Instruction m
addI = binaryI (+)

mulI :: MonadProgram r b m => Instruction m
mulI = binaryI (*)



-- execution

exec :: Monad m => (Int -> m (Maybe Int)) ->  m ()
exec f = fix (\e i -> whenJustM (f i) e) 0


step :: MonadProgram r b m => Int -> m (Maybe Int)
step index = do
  opCode <- get index
  case opCode of
    1  -> Just <$> addI index
    2  -> Just <$> mulI index
    99 -> return Nothing
    _  -> error $ "unexpected opCode: " ++ show opCode

debugStep :: MonadProgram r IO m => Int -> m (Maybe Int)
debugStep index = do
  liftBase . putStrLn =<< showProgram index
  step index


run :: V.Vector Int -> V.Vector Int
run program = runST $ do
  v <- V.thaw program
  runReaderT (exec step) $ Program v
  V.freeze v

runDebug :: V.Vector Int -> IO (V.Vector Int)
runDebug program = do
  v <- V.thaw program
  runReaderT (exec debugStep) $ Program v
  V.freeze v

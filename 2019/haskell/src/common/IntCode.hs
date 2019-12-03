{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module IntCode where



-- import


import           Control.Monad.Base
import           Control.Monad.Extra
import           Control.Monad.Reader
import           Control.Monad.ST
import           Data.Has
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as V



-- program

newtype Program s = Program (V.MVector s Int)

type MonadProgram r s m = ( MonadReader r m
                          , Has (Program s) r
                          , MonadBase (ST s) m
                          )



-- memory access

get :: MonadProgram r s m => Int -> m Int
get x = do
  Program v <- asks getter
  liftBase $ V.read v x

set :: MonadProgram r s m => Int -> Int -> m ()
set i x = do
  Program v <- asks getter
  liftBase $ V.write v i x



-- instructions

type Instruction m = Int -> m Int

addI :: MonadProgram r s m => Instruction m
addI index = do
  a <- get =<< get (index + 1)
  b <- get =<< get (index + 2)
  d <- get $ index + 3
  set d $ a + b
  return $ index + 4

mulI :: MonadProgram r s m => Instruction m
mulI index = do
  a <- get =<< get (index + 1)
  b <- get =<< get (index + 2)
  d <- get $ index + 3
  set d $ a * b
  return $ index + 4

step :: MonadProgram r s m => Int -> m (Maybe Int)
step index = do
  opCode <- get index
  case opCode of
    1  -> Just <$> addI index
    2  -> Just <$> mulI index
    99 -> return Nothing
    _  -> error $ "unexpected opCode: " ++ show opCode



-- execution

exec :: MonadProgram r s m => m ()
exec = exec_ 0
  where exec_ i = whenJustM (step i) exec_

run :: V.Vector Int -> V.Vector Int
run program = runST $ do
  v <- V.thaw program
  runReaderT exec $ Program v
  V.freeze v

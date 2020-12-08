{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}

-- import

import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Reader     as MR
import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Monad.State      as MS
import qualified Control.Monad.Writer     as MW
import           Data.Function            (on)
import qualified Data.IntSet              as S
import           Data.List
import           Data.List.NonEmpty       (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty       as NE
import           Data.Maybe
import qualified Data.Vector              as V
import           Text.Parsec              hiding ((<|>))
import           Text.Parsec.Char

import           AOC



-- program

type Program = V.Vector Instruction

data Instruction
  = Nop Int
  | Acc Int
  | Jmp Int
  deriving (Show, Eq)



-- parsing

parseInput :: String -> Program
parseInput = V.fromList . parseLinesWith instruction
  where instruction = choice [nopP, accP, jmpP] <?> "an instruction"
        nopP = symbol "nop" >> fmap Nop intLiteral
        accP = symbol "acc" >> fmap Acc intLiteral
        jmpP = symbol "jmp" >> fmap Jmp intLiteral



-- execution

data Register m a = Register
  { rGet :: m a
  , rSet :: a -> m ()
  }

eval :: Register m a -> m a
eval = rGet

(@=) :: Register m a -> a -> m ()
(@=) = rSet

(%=) :: Monad m => Register m a -> (a -> a) -> m ()
x %= f = rSet x . f =<< rGet x

(+=) :: (Monad m, Num a) => Register m a -> a -> m ()
x += n = x %= (+n)


class Monad m => MonadVM m where
  rPC     :: Register m Int
  rACC    :: Register m Int
  current :: m (Maybe Instruction)
  debug   :: String -> m ()


nop :: MonadVM m => m ()
nop = do
  rPC += 1

acc :: MonadVM m => Int -> m ()
acc n = do
  rACC += n
  rPC  += 1

jmp :: MonadVM m => Int -> m ()
jmp n = do
  rPC += n

step :: MonadVM m => m ()
step = do
  instruction <- current
  case instruction of
    Just (Nop _) -> nop
    Just (Acc n) -> acc n
    Just (Jmp n) -> jmp n
    Nothing      -> error "out of bounds"



-- storage

data VMState = VMState
  { vmPC  :: {-# UNPACK #-} !Int
  , vmAcc :: {-# UNPACK #-} !Int
  }

type VM = RWS.RWS Program [String] (NonEmpty VMState)

instance MonadVM VM where
  rPC  = Register
           (MS.gets $ vmPC  . NE.head)
           (\x -> MS.modify (\(s :| rest) -> s {vmPC  = x} :| rest))
  rACC = Register
           (MS.gets $ vmAcc . NE.head)
           (\x -> MS.modify (\(s :| rest) -> s {vmAcc = x} :| rest))
  current = do
    index <- eval rPC
    tape  <- MR.ask
    pure $ tape V.!? index
  debug s = MW.tell [s]

runVM :: Program -> VM a -> ([String], a)
runVM program exec = (logs, result)
  where (result, _, logs) = RWS.runRWS exec program $ pure $ VMState 0 0

duplicateState :: VM ()
duplicateState = do
  (s :| _) <- MS.get
  MS.modify $ (s <|)

popState :: VM ()
popState = do
  (_ :| rest) <- MS.get
  case rest of
    []    -> error "stack underflow"
    (a:b) -> MS.put $ a :| b



-- solution

data EndType = Loop | OutOfBounds
  deriving (Show, Eq)

runUntilEnd :: MonadVM m => m EndType
runUntilEnd = go S.empty
  where go !seen = do
          index <- eval rPC
          inst  <- current
          if | isNothing inst      -> pure OutOfBounds
             | S.member index seen -> pure Loop
             | otherwise           -> step >> go (S.insert index seen)

part1 :: Program -> ([String], Int)
part1 program = runVM program $ runUntilEnd >> eval rACC

part2 :: Program -> ([String], Int)
part2 program = runVM program $ go >> eval rACC
  where go = do
          inst  <- fromJust <$> current
          index <- eval rPC
          if isAcc inst
          then step >> go
          else do
            duplicateState
            end <- MR.local (switch index) runUntilEnd
            when (end == Loop) $ do
              popState
              step >> go
        isAcc (Acc _) = True
        isAcc _       = False
        switch index prog = case prog V.! index of
          Nop n -> prog V.// [(index, Jmp n)]
          Jmp n -> prog V.// [(index, Nop n)]
          _     -> error "tried to flip an acc instruction"



-- main

main :: IO ()
main = aocMain 8 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  runAndPrint part1 testInput
  runAndPrint part1 realInput
  putStrLn "# Part 2"
  runAndPrint part2 testInput
  runAndPrint part2 realInput

runAndPrint fun input = do
  let (logs, result) = fun input
  putStrLn $ "acc: " ++ show result
  putStr $ unlines logs

example :: String
example = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6"

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp           #-}
{-# LANGUAGE TemplateHaskell            #-}

module IntCode (run, debug, runConcurrently) where



-- import

import           Control.Lens                hiding (index, ( # ), (...))
import           Control.Monad.Extra
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State
import           Data.List                   (intercalate, transpose)
import           Data.List.Split             (chunksOf)
import           Data.Maybe
import           Data.Traversable            (for)
import qualified Data.Vector                 as BV
import qualified Data.Vector.Unboxed         as V hiding (length)
import qualified Data.Vector.Unboxed.Mutable as V hiding (replicate)
import           Prelude                     hiding (read)
import           Text.Printf

import           AOC.Debug.Color
import           AOC.Misc



-- capabilities

class Monad m => MonadIntCode m where
  readTape  :: Int -> m Int
  writeTape :: Int -> Int -> m ()
  input     :: m (Maybe Int)
  output    :: Int -> m ()
  getBase   :: m Int
  addToBase :: Int -> m ()



-- memory access

data Mode = Immediate
          | Position
          | Relative
          deriving (Show, Eq)

readAs :: MonadIntCode m => Mode -> Int -> m Int
readAs Immediate = readTape
readAs Position  = readTape <=< readTape
readAs Relative  = readTape <=< readRelative
  where readRelative pos = liftM2 (+) getBase $ readTape pos

writeAs :: MonadIntCode m => Mode -> Int -> Int -> m ()
writeAs Immediate _ _ = error "write: immediate mode"
writeAs Position  p x = readTape     p >>= \i -> writeTape i x
writeAs Relative  p x = readRelative p >>= \i -> writeTape i x
  where readRelative pos = liftM2 (+) getBase $ readTape pos



-- instructions

type Instruction m = Int -> (Mode,Mode,Mode) -> m Int

binaryI :: MonadIntCode m => (Int -> Int -> Int) -> Instruction m
binaryI (#) index (m1,m2,m3) = do
  a <- readAs m1 $ index + 1
  b <- readAs m2 $ index + 2
  writeAs m3 (index + 3) $ a # b
  return $ index + 4

addI :: MonadIntCode m => Instruction m
addI = binaryI (+)

mulI :: MonadIntCode m => Instruction m
mulI = binaryI (*)

ltI :: MonadIntCode m => Instruction m
ltI = binaryI $ fromEnum ... (<)

eqI :: MonadIntCode m => Instruction m
eqI = binaryI $ fromEnum ... (==)


inI :: MonadIntCode m => Instruction m
inI index (m,_,_) = do
  value <- input
  case value of
    Nothing -> return index
    Just x  -> do
      writeAs m (index + 1) x
      return $ index + 2

outI :: MonadIntCode m => Instruction m
outI index (m,_,_) = do
  output =<< readAs m (index + 1)
  return $ index + 2


jumpI :: MonadIntCode m => (Int -> Bool) -> Instruction m
jumpI shouldJump index (m1,m2,_) = do
  a <- readAs m1 $ index + 1
  if shouldJump a
    then readAs m2 $ index + 2
    else return $ index + 3

jmpTI :: MonadIntCode m => Instruction m
jmpTI = jumpI (/= 0)

jmpFI :: MonadIntCode m => Instruction m
jmpFI = jumpI (== 0)


rbI :: MonadIntCode m => Instruction m
rbI index (m,_,_) = do
  addToBase =<< readAs m (index + 1)
  return $ index + 2



-- execution

step :: MonadIntCode m => Int -> m (Maybe Int)
step index = do
  (opCode, modes) <- splitAt 2 . reverse . show <$> readTape index
  let [m1,m2,m3] = take 3 $ map toMode modes ++ repeat Position
  case take 2 $ opCode ++ "0" of
    "10" -> Just <$> addI  index (m1,m2,m3)
    "20" -> Just <$> mulI  index (m1,m2,m3)
    "30" -> Just <$> inI   index (m1,m2,m3)
    "40" -> Just <$> outI  index (m1,m2,m3)
    "50" -> Just <$> jmpTI index (m1,m2,m3)
    "60" -> Just <$> jmpFI index (m1,m2,m3)
    "70" -> Just <$> ltI   index (m1,m2,m3)
    "80" -> Just <$> eqI   index (m1,m2,m3)
    "90" -> Just <$> rbI   index (m1,m2,m3)
    "99" -> return Nothing
    _    -> error $ "unexpected opCode: " ++ opCode
  where toMode '0' = Position
        toMode '1' = Immediate
        toMode '2' = Relative
        toMode c   = error $ "unexpected mode: " ++ [c]



-- vm

type Tape m = V.MVector (PrimState m) Int

data VMData m = VMData { _programTape  :: Tape m
                       , _inputBuffer  :: [Int]
                       , _outputBuffer :: [Int]
                       , _relativeBase :: Int
                       }
makeLenses ''VMData

newtype VM m a = VM { runVM :: StateT (VMData m) m a }
  deriving (Functor, Applicative, Monad, MonadState (VMData m), MonadIO)


vmReadTape :: PrimMonad m => VMData m -> Int -> m Int
vmReadTape d = V.read $ d ^. programTape

vmWriteTape :: PrimMonad m => VMData m -> Int -> Int -> m (Tape m)
vmWriteTape d index value = do
  let v = d ^. programTape
  if index < V.length v
  then V.write v index value >> return v
  else do
    let currentLength = V.length v
        targetLength  = 2 ^ ceiling (logBase 2 $ fromIntegral $ currentLength + 1)
    newTape <- V.grow v $ targetLength - currentLength
    V.write newTape index value
    return newTape

vmInput :: VMData m -> (Maybe Int, VMData m)
vmInput d = case d ^. inputBuffer of
  []     -> (Nothing, d)
  (x:xs) -> (Just x, d & inputBuffer .~ xs)


instance PrimMonad m => MonadIntCode (VM m) where
  readTape index = do
    tape <- get
    VM $ lift $ vmReadTape tape index
  writeTape index value = do
    tape <- get
    newTape <- VM $ lift $ vmWriteTape tape index value
    modify $ set programTape newTape

  getBase = use relativeBase
  addToBase delta = relativeBase += delta

  input = state vmInput
  output x = outputBuffer %= (x:)


runInVM :: PrimMonad m => Tape m -> [Int] -> VM m () -> m (VMData m)
runInVM tape iBuffer = runS (VMData tape iBuffer [] 0) . runVM
  where runS = flip execStateT



-- execution

exec :: Monad m => (Int -> m (Maybe Int)) ->  m ()
exec f = fix (\e i -> whenJustM (f i) e) 0

run :: V.Vector Int -> [Int] -> (V.Vector Int, [Int])
run tape iBuffer = runST $ do
  mv <- V.thaw tape
  d  <- runInVM mv iBuffer $ exec step
  pt <- V.freeze $ d ^. programTape
  return (pt, reverse $ d ^. outputBuffer)



-- debug

debug :: V.Vector Int -> [Int] -> IO ()
debug tape iBuffer = do
  mv <- V.thaw tape
  void $ runInVM mv iBuffer $ exec $ \i -> do
    liftIO . putStrLn =<< showVM i
    step i

showVM :: Int -> VM IO String
showVM index = VM $ do
  VMData mv i o _ <- get
  v <- lift $ V.toList <$> V.freeze mv
  let chunks  = chunksOf 8 v
      widths    = map (maximum . map (length . show)) $ transpose chunks
      display r c x
        | r*8 + c /= index =                make c x
        | otherwise        = bgColor blue $ make c x
      make c = printf $ "%" ++ show (widths !! c) ++ "d"
  return $ unlines $ [ "input  buffer: " ++ show i
                     , "output buffer: " ++ show (reverse o)
                     ] ++ [ printf "[%s]" $ intercalate ", " [ display r c x
                                                             | c <- [0..]
                                                             | x <- line
                                                             ]
                          | r    <- [0..]
                          | line <- chunks
                          ]



-- concurrent vm

data ConcurrentVMData m = CVMData { _vms            :: BV.Vector (VMData m)
                                  , _instPointers   :: BV.Vector (Maybe Int)
                                  , _currentProgram :: Int
                                  }
makeLenses ''ConcurrentVMData

newtype ConcurrentVM m a = CVM { runCVM :: StateT (ConcurrentVMData m) m a }
  deriving (Functor, Applicative, Monad, MonadState (ConcurrentVMData m), MonadIO)


programAfterCurrent :: PrimMonad m => ConcurrentVM m Int
programAfterCurrent = do
  p <- use currentProgram
  n <- uses vms BV.length
  return $ mod (p+1) n

goToNextProgram :: PrimMonad m => ConcurrentVM m ()
goToNextProgram = do
  p <- programAfterCurrent
  currentProgram .= p

setInstructionPointer :: PrimMonad m => Maybe Int -> ConcurrentVM m ()
setInstructionPointer x = do
  p <- use currentProgram
  instPointers %= (BV.// [(p, x)])

getCurrentVM :: Monad m => ConcurrentVM m (VMData m)
getCurrentVM = do
  p <- use currentProgram
  gets (^?! vms . ix p)


instance PrimMonad m => MonadIntCode (ConcurrentVM m) where
  readTape index = do
    vm <- getCurrentVM
    CVM $ lift $ vmReadTape vm index
  writeTape index value = do
    tape <- getCurrentVM
    p    <- use currentProgram
    newTape <- CVM $ lift $ vmWriteTape tape index value
    vms . ix p . programTape .= newTape

  getBase = view relativeBase <$> getCurrentVM
  addToBase delta = do
    p <- use currentProgram
    modify $ over (vms . ix p . relativeBase) (+delta)

  input = do
    p            <- use currentProgram
    (res, newVM) <- vmInput <$> gets (^?! vms . ix p)
    vms . ix p .= newVM
    return res
  output x = do
    p <- use currentProgram
    vms . ix p . outputBuffer %= (x:)


runInConcurrentVM :: PrimMonad m => [(Tape m, [Int])] -> ConcurrentVM m () -> m [VMData m]
runInConcurrentVM v = fmap (BV.toList . (^.  vms)) . runS baseState . runCVM
  where runS = flip execStateT
        baseState = CVMData vmsData basePointers 0
        basePointers = BV.fromList $ Just 0 <$ v
        vmsData = BV.fromList [VMData tape iBuffer [] 0 | (tape, iBuffer) <- v]

execConcurrently :: PrimMonad m => ConcurrentVM m ()
execConcurrently = do
  prog         <- use currentProgram
  maybePointer <- gets (^?! instPointers . ix prog)
  case maybePointer of
    Nothing -> do
      goToNextProgram
      execConcurrently
    Just ip -> do
      np <- step ip
      setInstructionPointer np
      case np of
        Nothing -> unlessM (uses instPointers $ all isNothing) $ do
          goToNextProgram
          execConcurrently
        Just x  -> do
          when (x /= ip) goToNextProgram
          execConcurrently

runConcurrently :: [([Int], [Int])] -> [(V.Vector Int, [Int])]
runConcurrently vmsData = runST $ do
  d <- for vmsData $ \(prog, ibuf) -> do
    tape <- V.thaw $ V.fromList prog
    return (tape, ibuf)
  res <- runInConcurrentVM d execConcurrently
  for res $ \vmData -> do
    v <- V.freeze $ vmData ^. programTape
    return (v, vmData ^. outputBuffer)

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module IntCode ( run
               , runM
               , runConcurrently
               , runConcurrentlyM
               , debug
               ) where



-- import

import           Control.Lens                hiding (index, ( # ), (...))
import           Control.Monad.Extra
import           Control.Monad.Primitive
import           Control.Monad.Reader
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




-- ##########################################
-- Part 1: abstract instructions


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




-- ##########################################
-- Part 2: single program vm

type Tape   m = V.MVector (PrimState m) Int
type Input  m = m (Maybe Int)
type Output m = Int -> m ()

data IOFuns m = IOFuns { inF  :: Input  m
                       , outF :: Output m
                       }
data VMData m = VMData { _programTape  :: Tape m
                       , _relativeBase :: Int
                       }
makeLenses ''VMData

newtype VM m a = VM { runVM :: ReaderT (IOFuns m) (StateT (VMData m) m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (IOFuns m)
           , MonadState  (VMData m)
           , MonadIO
           )


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

vmLift :: Monad m => m a -> VM m a
vmLift = VM . lift . lift


instance PrimMonad m => MonadIntCode (VM m) where
  readTape index = do
    vm <- get
    vmLift $ vmReadTape vm index
  writeTape index value = do
    vm <- get
    newTape <- vmLift $ vmWriteTape vm index value
    modify $ set programTape newTape

  getBase = use relativeBase
  addToBase delta = relativeBase += delta

  input = vmLift =<< asks inF
  output x = vmLift . ($ x) =<< asks outF


runInVM :: PrimMonad m => Tape m -> Input m -> Output m -> VM m () -> m (VMData m)
runInVM tape iF oF = runS . runR . runVM
  where runS = flip execStateT $ VMData tape 0
        runR = flip runReaderT $ IOFuns iF oF



-- io 1: State with mutable input buffer

inS1 :: MonadState ([Int], [Int]) m => m (Maybe Int)
inS1 = do
  ib <- use _1
  case ib of
    []     -> return Nothing
    (x:xs) -> do
      _1 .= xs
      return $ Just x

outS1 :: MonadState ([Int], [Int]) m => Int -> m ()
outS1 x = _2 %= (x:)

runS1 :: Monad m => [Int] -> StateT ([Int], [Int]) m a -> m (a, [Int])
runS1 ib e = fmap snd <$> runStateT e (ib, [])



-- execution

exec :: Monad m => (Int -> m (Maybe Int)) ->  m ()
exec f = fix (\e i -> whenJustM (f i) e) 0

run :: V.Vector Int -> [Int] -> (V.Vector Int, [Int])
run t ib = runST $ runS1 ib $ runM t inS1 outS1

runM :: PrimMonad m => V.Vector Int -> Input m -> Output m -> m (V.Vector Int)
runM tape iF oF = do
  mv <- V.thaw tape
  d  <- runInVM mv iF oF $ exec step
  V.freeze $ d ^. programTape




-- ##########################################
-- Part 3: concurrent vm

data ConcurrentIOFuns m = CIOFuns { inCF  :: Int -> Input  m
                                  , outCF :: Int -> Output m
                                  }
data ConcurrentVMData m = CVMData { _vms            :: BV.Vector (VMData m)
                                  , _instPointers   :: BV.Vector (Maybe Int)
                                  , _currentProgram :: Int
                                  }
makeLenses ''ConcurrentVMData

newtype ConcurrentVM m a =
  CVM { runCVM :: ReaderT (ConcurrentIOFuns m) (StateT (ConcurrentVMData m) m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (ConcurrentIOFuns m)
           , MonadState  (ConcurrentVMData m)
           , MonadIO
           )


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

liftCVM :: Monad m => m a -> ConcurrentVM m a
liftCVM = CVM . lift . lift


instance PrimMonad m => MonadIntCode (ConcurrentVM m) where
  readTape index = do
    vm <- getCurrentVM
    liftCVM $ vmReadTape vm index
  writeTape index value = do
    tape <- getCurrentVM
    p    <- use currentProgram
    newTape <- liftCVM $ vmWriteTape tape index value
    vms . ix p . programTape .= newTape

  getBase = view relativeBase <$> getCurrentVM
  addToBase delta = do
    p <- use currentProgram
    modify $ over (vms . ix p . relativeBase) (+delta)

  input = do
    p <- use currentProgram
    f <- asks inCF
    liftCVM $ f p
  output x = do
    p <- use currentProgram
    f <- asks outCF
    liftCVM $ f p x


runInConcurrentVM :: PrimMonad m       =>
                     [Tape m]          ->
                     (Int -> Input  m) ->
                     (Int -> Output m) ->
                     ConcurrentVM m () -> m [VMData m]
runInConcurrentVM tapes iF oF = fmap (BV.toList . (^.  vms)) . runS . runR . runCVM
  where runS = flip execStateT $ CVMData vmsData basePointers 0
        runR = flip runReaderT $ CIOFuns iF oF
        basePointers = BV.fromList $ Just 0 <$ tapes
        vmsData = BV.fromList [VMData tape 0 | tape <- tapes]



-- io 2: State with indexed input buffer

inS2 :: MonadState [([Int], [Int], Int)] m => Int -> m (Maybe Int)
inS2 p = Just <$> liftM2 (!!) (gets (^?! ix p . _1)) (gets (^?! ix p . _3))

outS2 :: MonadState [([Int], [Int], Int)] m => Int -> Int -> m ()
outS2 p x = ix p . _2 %= (x:)

runS2 :: Monad m => [[Int]] -> StateT [([Int], [Int], Int)] m a -> m [[Int]]
runS2 ibs e = map (^. _2) <$> execStateT e [(ib, [], 0) | ib <- ibs]



-- execution

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

runConcurrentlyM :: PrimMonad m       =>
                    [V.Vector Int]    ->
                    (Int -> Input  m) ->
                    (Int -> Output m) -> m [V.Vector Int]
runConcurrentlyM vmsData iF oF = do
  tapes <- traverse V.thaw vmsData
  res   <- runInConcurrentVM tapes iF oF execConcurrently
  for res $ \vmData -> V.freeze $ vmData ^. programTape

runConcurrently :: [(V.Vector Int, [Int])] -> [[Int]]
runConcurrently i = runST $ runS2 ibs $ runConcurrentlyM tapes inS2 outS2
  where (tapes, ibs) = unzip i




-- ##########################################
-- Part 4: debug

debug :: V.Vector Int -> [Int] -> IO ()
debug tape iBuffer = do
  mv <- V.thaw tape
  void $ runS1 iBuffer $ runInVM mv inS1 outS1 $ exec $ \i -> do
    liftIO . putStrLn =<< showVM i
    step i

showVM :: PrimMonad m => Int -> VM m String
showVM index = VM $ do
  VMData mv _ <- get
  v <- lift $ V.toList <$> V.freeze mv
  let chunks  = chunksOf 8 v
      widths    = map (maximum . map (length . show)) $ transpose chunks
      display r c x
        | r*8 + c /= index =                make c x
        | otherwise        = bgColor blue $ make c x
      make c = printf $ "%" ++ show (widths !! c) ++ "d"
  return $ unlines [ printf "[%s]" $ intercalate ", " [ display r c x
                                                      | c <- [0..]
                                                      | x <- line
                                                      ]
                   | r    <- [0..]
                   | line <- chunks
                   ]

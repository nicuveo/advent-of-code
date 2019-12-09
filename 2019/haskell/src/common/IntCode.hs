{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ParallelListComp           #-}

module IntCode (run, debug, runConcurrently) where



-- import

import           Control.Monad.Extra
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State
import           Data.List                   (intercalate, transpose)
import           Data.List.Split             (chunksOf)
import qualified Data.Vector                 as BV
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as V hiding (replicate)
import           Prelude                     hiding (read)
import           Text.Printf

import           AOC.Debug.Color
import           AOC.Misc



-- capabilities

class Monad m => MonadTape m where
  read  :: Int -> m Int
  write :: Int -> Int -> m ()

class Monad m => MonadInteract m where
  input  :: m (Maybe Int)
  output :: Int -> m ()



-- memory access

data Mode = Immediate | Position deriving (Show, Eq)

readAs :: MonadTape m => Mode -> Int -> m Int
readAs Immediate = read
readAs Position  = read >=> read

writeAt :: MonadTape m => Int -> Int -> m ()
writeAt p x = read p >>= \i -> write i x



-- instructions

type Instruction m = Int -> [Mode] -> m Int

binaryI :: MonadTape m => (Int -> Int -> Int) -> Instruction m
binaryI (#) index (m1:m2:_) = do
  a <- readAs m1 $ index + 1
  b <- readAs m2 $ index + 2
  writeAt (index + 3) $ a # b
  return $ index + 4
binaryI _ _ _ = error "binaryI: empty mode list"

addI :: MonadTape m => Instruction m
addI = binaryI (+)

mulI :: MonadTape m => Instruction m
mulI = binaryI (*)

ltI :: MonadTape m => Instruction m
ltI = binaryI $ fromEnum ... (<)

eqI :: MonadTape m => Instruction m
eqI = binaryI $ fromEnum ... (==)


inI :: (MonadTape m, MonadInteract m) => Instruction m
inI index _ = do
  value <- input
  case value of
    Nothing -> return index
    Just x  -> do
      writeAt (index + 1) x
      return $ index + 2

outI :: (MonadTape m, MonadInteract m) => Instruction m
outI _ [] = error "outI: empty mode list"
outI index (m:_) = do
  output =<< readAs m (index + 1)
  return $ index + 2


jumpI :: MonadTape m => (Int -> Bool) -> Instruction m
jumpI shouldJump index (m1:m2:_) = do
  a <- readAs m1 $ index + 1
  if shouldJump a
    then readAs m2 $ index + 2
    else return $ index + 3
jumpI _ _ _ = error "jumpI: empty mode list"

jmpTI :: MonadTape m => Instruction m
jmpTI = jumpI (/= 0)

jmpFI :: MonadTape m => Instruction m
jmpFI = jumpI (== 0)



-- execution

exec :: Monad m => (Int -> m (Maybe Int)) ->  m ()
exec f = fix (\e i -> whenJustM (f i) e) 0

step :: (MonadTape m, MonadInteract m) => Int -> m (Maybe Int)
step index = do
  (opCode, modes) <- splitAt 2 . reverse . show <$> read index
  let ms = map toMode modes ++ repeat Position
  case take 2 $ opCode ++ "0" of
    "10" -> Just <$> addI  index ms
    "20" -> Just <$> mulI  index ms
    "30" -> Just <$> inI   index ms
    "40" -> Just <$> outI  index ms
    "50" -> Just <$> jmpTI index ms
    "60" -> Just <$> jmpFI index ms
    "70" -> Just <$> ltI   index ms
    "80" -> Just <$> eqI   index ms
    "99" -> return Nothing
    _    -> error $ "unexpected opCode: " ++ opCode
  where toMode '0' = Position
        toMode '1' = Immediate
        toMode c   = error $ "unexpected mode: " ++ [c]



-- vm

newtype Tape m = Tape (V.MVector (PrimState m) Int)

data IOData = IOData { inputBuffer  :: [Int]
                     , outputBuffer :: [Int]
                     }

newtype VM m a = VM { runVM :: StateT IOData (ReaderT (Tape m) m) a }
  deriving (Functor, Applicative, Monad, MonadIO)


instance PrimMonad m => MonadTape (VM m) where
  read index = VM $ do
    Tape v <- ask
    lift $ lift $ V.read v index
  write index value = VM $ do
    Tape v <- ask
    lift $ lift $ V.write v index value

instance Monad m => MonadInteract (VM m) where
  input = VM $ state $ \iod@(IOData (x:xs) _) -> ( Just x
                                                 , iod {inputBuffer = xs}
                                                 )
  output x = VM $ modify $ \iod -> iod {outputBuffer = x : outputBuffer iod}


runInVM :: PrimMonad m => Tape m -> [Int] -> VM m () -> m [Int]
runInVM tape iBuffer = fmap outputBuffer        .
                       runR tape                .
                       runS (IOData iBuffer []) .
                       runVM
  where runR = flip runReaderT
        runS = flip execStateT



-- execution

run :: V.Vector Int -> [Int] -> (V.Vector Int, [Int])
run tape iBuffer = runST $ do
  mv <- V.thaw tape
  o  <- runInVM (Tape mv) iBuffer $ exec step
  v  <- V.freeze mv
  return (v, reverse o)



-- debug

debug :: V.Vector Int -> [Int] -> IO ()
debug tape iBuffer = do
  mv <- V.thaw tape
  void $ runInVM (Tape mv) iBuffer $ exec $ \i -> do
    liftIO . putStrLn =<< showVM i
    step i

showVM :: Int -> VM IO String
showVM index = VM $ do
  Tape mv    <- ask
  IOData i o <- get
  v <- lift $ lift $ V.toList <$> V.freeze mv
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

newtype Tapes m =
  Tapes { getTapes :: BV.Vector (V.MVector (PrimState m) Int) }

data ConcurrentInfo = CI { inputBuffers        :: BV.Vector [Int]
                         , currentProgram      :: Int
                         , instructionPointers :: BV.Vector (Maybe Int)
                         }

newtype ConcurrentVM m a =
  CVM { runCVM :: StateT ConcurrentInfo (ReaderT (Tapes m) m) a }
  deriving ( Functor, Applicative, Monad
           , MonadState ConcurrentInfo
           , MonadReader (Tapes m)
           , MonadIO
           )


programAfterCurrent :: PrimMonad m => ConcurrentVM m Int
programAfterCurrent = do
  p <- gets currentProgram
  n <- asks $ BV.length . getTapes
  return $ mod (p+1) n

goToNextProgram :: PrimMonad m => ConcurrentVM m ()
goToNextProgram = do
  p <- programAfterCurrent
  modify $ \s -> s { currentProgram = p }

setInstructionPointer :: PrimMonad m => Maybe Int -> ConcurrentVM m ()
setInstructionPointer x = do
  p  <- gets currentProgram
  ps <- gets instructionPointers
  modify $ \s -> s { instructionPointers = ps BV.// [(p,x)]}


instance PrimMonad m => MonadTape (ConcurrentVM m) where
  read index = CVM $ do
    p <- gets currentProgram
    v <- asks $ (BV.! p) . getTapes
    lift $ lift $ V.read v index
  write index value = CVM $ do
    p <- gets currentProgram
    v <- asks $ (BV.! p) . getTapes
    lift $ lift $ V.write v index value

instance PrimMonad m => MonadInteract (ConcurrentVM m) where
  input = CVM $ do
    p  <- gets currentProgram
    ib <- gets $ (BV.! p) . inputBuffers
    case ib of
      []     -> return Nothing
      (x:xs) -> do
        modify $ \s -> s {inputBuffers = inputBuffers s BV.// [(p,xs)]}
        return $ Just x
  output x = do
    target <- programAfterCurrent
    modify $ \s ->
      s {inputBuffers = inputBuffers s BV.// [
            (target, (inputBuffers s BV.! target) ++ [x])
            ]}


runInConcurrentVM :: PrimMonad m        =>
                     Tapes m            ->
                     BV.Vector [Int]    ->
                     ConcurrentVM m Int ->
                     m Int
runInConcurrentVM tapes iBuffers = runR tapes . runS baseState . runCVM
  where runR = flip runReaderT
        runS = flip evalStateT
        baseState = CI iBuffers 0 $ BV.map (const $ Just 0) iBuffers

stepConcurrent :: PrimMonad m => ConcurrentVM m (Maybe Int)
stepConcurrent = do
  prog         <- gets currentProgram
  maybePointer <- gets $ (BV.! prog) . instructionPointers
  case maybePointer of
    Nothing -> do
      out <- gets $ (BV.! prog) . inputBuffers
      return $ Just $ head out
    Just ip -> do
      np <- step ip
      case np of
        Nothing -> do
          setInstructionPointer Nothing
          goToNextProgram
        Just x  -> if x /= ip
                   then setInstructionPointer $ Just x
                   else goToNextProgram
      return Nothing

execConcurrently :: PrimMonad m => ConcurrentVM m Int
execConcurrently = do
  mResult <- stepConcurrent
  case mResult of
    Nothing -> execConcurrently
    Just  x -> return x

runConcurrently :: V.Vector Int -> BV.Vector [Int] -> Int
runConcurrently prog ibs = runST $ do
  let n = BV.length ibs
  tapes <- fmap Tapes $ traverse V.thaw $ BV.replicate n prog
  runInConcurrentVM tapes ibs execConcurrently

{-# LANGUAGE FlexibleContexts #-}

module AOC.Debug.Animate where



-- imports

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Extra        hiding (whileM)
import           Control.Monad.Loops        (whileM, whileM_)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Maybe
import           Data.Time.Clock
import           Data.Word
import           System.IO
import           Text.Printf



-- color

data Color = RGB Word8 Word8 Word8

fgColor :: Color -> String -> String
fgColor (RGB r g b) = printf "\ESC[38;2;%d;%d;%dm%s\ESC[0m" r g b

bgColor :: Color -> String -> String
bgColor (RGB r g b) = printf "\ESC[48;2;%d;%d;%dm%s\ESC[0m" r g b

red, yellow, green, cyan, blue, magenta :: Color
red     = RGB 255   0   0
yellow  = RGB 255 255   0
green   = RGB   0 255   0
cyan    = RGB   0 255 255
blue    = RGB   0   0 255
magenta = RGB 255   0 255



-- screen clearing

type ScreenAction = IO ()

clearScreen :: ScreenAction
clearScreen = putStr "\ESC[2J"

resetCursor :: ScreenAction
resetCursor = putStr "\ESC[;H"



-- animation delay

type Milliseconds = Int

defaultDelay :: Milliseconds
defaultDelay = 1000



-- animation data

type Logs          = [String]
type RenderFun a   = a -> String
type UpdateFun a m = a -> m (Maybe (Logs, a))

data AnimParams a m = AP { renderFun :: RenderFun a
                         , updateFun :: UpdateFun a m
                         , origDelay :: Int
                         , origState :: m (Logs, a)
                         }

data AnimState a = AS { currentDelay :: Milliseconds
                      , stateHistory :: [(Logs, a)]
                      , currentIndex :: Int
                      , isPaused     :: Bool
                      , canContinue  :: Bool
                      , frameCount   :: Int
                      }

mkAnimState :: Milliseconds -> (Logs, a) -> AnimState a
mkAnimState d la = AS d [la] 0 False True 0

type MonadAnim a m = ReaderT (AnimParams a m) (StateT (AnimState a) m)

liftAnim :: Monad m => m x -> MonadAnim a m x
liftAnim = lift . lift



-- animation functions

getCurrent :: Monad m => MonadAnim a m (Logs, a)
getCurrent = liftA2 (!!) hist index
  where hist  = gets stateHistory
        index = gets currentIndex

computeNext :: Monad m => MonadAnim a m ()
computeNext = do
  index <- gets currentIndex
  when (index == 0) $ whenJustM doCompute appendState
  where doCompute = liftAnim =<< asks updateFun <*> fmap snd getCurrent

appendState :: Monad m => (Logs, a) -> MonadAnim a m ()
appendState a = do
  index <- gets currentIndex
  assert (index == 0) $ modify $ \s ->
    s { currentIndex = 1
      , stateHistory = a : take 20 (stateHistory s)
      }

stepForwards :: Monad m => MonadAnim a m ()
stepForwards = do
  computeNext
  modify $ \s ->
    s { frameCount   = frameCount   s + 1
      , currentIndex = currentIndex s - 1
      }

stepBackwards :: Monad m => MonadAnim a m ()
stepBackwards = do
  index <- gets currentIndex
  limit <- length <$> gets stateHistory
  when (index < limit - 1) $ modify $ \s ->
    s { frameCount   = frameCount   s - 1
      , currentIndex = currentIndex s + 1
      }

restart :: Monad m => MonadAnim a m ()
restart = do
  a <- liftAnim =<< asks origState
  modify $ \s -> s { stateHistory = [a]
                   , currentIndex = 0
                   , frameCount   = 0
                   }



-- keyboard handling

data Action = Pause
            | Faster
            | Slower
            | ResetSpeed
            | Restart
            | PrevFrame
            | NextFrame
            | Help
            | Quit
            deriving Show

peekActions :: IO [Action]
peekActions = fmap catMaybes $ whileM (hReady stdin) $ do
  c <- getChar
  return  $ case c of
              ' ' -> Just Pause
              '+' -> Just Faster
              '-' -> Just Slower
              '=' -> Just ResetSpeed
              'r' -> Just Restart
              'p' -> Just PrevFrame
              'n' -> Just NextFrame
              '?' -> Just Help
              'q' -> Just Quit
              _   -> Nothing



-- time

elapsedSince :: MonadIO m => UTCTime -> m Milliseconds
elapsedSince t = liftIO $ do
  now <- getCurrentTime
  return $ ceiling $ 1000 * diffUTCTime now t

timeRemaining :: MonadIO m => UTCTime -> MonadAnim a m Milliseconds
timeRemaining startTime = do
  elapsed <- elapsedSince startTime
  delta   <- gets currentDelay
  return $ max 0 $ delta - elapsed

canWait :: MonadIO m => UTCTime -> MonadAnim a m Bool
canWait startTime = do
  elapsed <- elapsedSince startTime
  delta   <- gets currentDelay
  return $ elapsed < delta



-- animate

printHelp :: MonadIO m => MonadAnim a m ()
printHelp = liftIO $ do
  clearScreen >> resetCursor
  putStr $ unlines [ "AOC animator deluxe keyboard shortcuts."
                   , ""
                   , "at any time:"
                   , "  <space> pause or resume"
                   , "  +       speed up"
                   , "  -       speed down"
                   , "  =       reset original speed"
                   , "  ?       print this help"
                   , "  q       quit"
                   , ""
                   , "when paused:"
                   , "  r       restart from original state"
                   , "  p       previous state (- 1 frame)"
                   , "  n           next state (+ 1 frame)"
                   , ""
                   , "(press any key to continue)"
                   ]
  void getChar

renderFrame :: MonadIO m => MonadAnim a m ()
renderFrame = do
  (logs, a) <- getCurrent
  frameData <- ($ a) <$> asks renderFun
  paused    <- gets isPaused
  delay     <- gets currentDelay
  n         <- gets frameCount
  liftIO $ do
    clearScreen
    resetCursor
    let pl = if paused then " (paused)" else ""
    printf "Interval: %dms%s\nFrame: #%d\n%s%s" delay pl n frameData $ unlines logs

processActions :: MonadIO m => MonadAnim a m ()
processActions = mapM_ exec =<< liftIO peekActions
  where
    exec Pause      = do
      modify (\s -> s { isPaused = not $ isPaused s })
      renderFrame
    exec Faster     = do
      modify (\s -> s { currentDelay = max   20 $ round $ fromIntegral (currentDelay s) * 0.80 })
      whenM (gets isPaused) renderFrame
    exec Slower     = do
      modify (\s -> s { currentDelay = min 2000 $ round $ fromIntegral (currentDelay s) * 1.25 })
      whenM (gets isPaused) renderFrame
    exec ResetSpeed = do
      value <- asks origDelay
      modify $ \s -> s { currentDelay = value }
      whenM (gets isPaused) renderFrame
    exec Restart    = whenM (gets isPaused) $ restart       >> renderFrame
    exec PrevFrame  = whenM (gets isPaused) $ stepBackwards >> renderFrame
    exec NextFrame  = whenM (gets isPaused) $ stepForwards  >> renderFrame
    exec Help       = printHelp >> renderFrame
    exec Quit       = modify $ \s -> s { canContinue = False }

animate :: MonadIO m => Milliseconds -> RenderFun a -> UpdateFun a m -> m (Logs, a) -> m ()
animate delay render step initialState = do
  liftIO $ do
    hSetBuffering stdin NoBuffering
    hSetEcho stdout False
  is <- initialState
  let animS = mkAnimState delay is
      animP = AP render step delay initialState
  flip evalStateT animS $ runReaderT run animP
  where
    run = do
      renderFrame
      frameStart <- liftIO getCurrentTime
      computeNext
      processActions
      whileM_ (gets canContinue &&^ (gets isPaused ||^ canWait frameStart)) $ do
        waitTime <- ifM (gets isPaused) (return (-1)) $ timeRemaining frameStart
        whenM (liftIO $ hWaitForInput stdin waitTime) processActions
      whenM (gets canContinue) $ do
        stepForwards
        run

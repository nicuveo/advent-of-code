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
import           System.IO
import           Text.Printf



-- screen clearing

type ScreenAction = IO ()

clearLine :: ScreenAction
clearLine = putStr "\ESC[K"

clearScreen :: ScreenAction
clearScreen = putStr "\ESC[2J"

resetCursor :: ScreenAction
resetCursor = putStr "\ESC[;H"

clearAndReset :: ScreenAction
clearAndReset = clearScreen >> resetCursor



-- animation delay

type Milliseconds = Int

defaultDelay :: Milliseconds
defaultDelay = 1000



-- animation data

type Logs          = [String]
type FrameData     = String
type RenderFun a   = a -> FrameData
type UpdateFun a m = a -> m (Maybe (Logs, a))

data AnimParams a m = AP { renderFun :: RenderFun a
                         , updateFun :: UpdateFun a m
                         , clearFun  :: ScreenAction
                         , origDelay :: Int
                         , origState :: m (Logs, a)
                         }

data AnimState a = AS { currentDelay :: Milliseconds
                      , stateHistory :: [(Logs, FrameData)]
                      , currentIndex :: Int
                      , isPaused     :: Bool
                      , canContinue  :: Bool
                      , frameCount   :: Int
                      , lastState    :: a
                      }

mkAnimState :: RenderFun a -> Milliseconds -> (Logs, a) -> AnimState a
mkAnimState r d (l, a) = AS d [(l, r a)] 0 False True 0 a

type MonadAnim a m = ReaderT (AnimParams a m) (StateT (AnimState a) m)

liftAnim :: Monad m => m x -> MonadAnim a m x
liftAnim = lift . lift



-- animation functions

getCurrent :: Monad m => MonadAnim a m (Logs, FrameData)
getCurrent = liftA2 (!!) hist index
  where hist  = gets stateHistory
        index = gets currentIndex

computeNext :: Monad m => MonadAnim a m ()
computeNext = do
  index <- gets currentIndex
  when (index == 0) $ whenJustM doCompute appendState
  where doCompute = liftAnim =<< asks updateFun <*> gets lastState

appendState :: Monad m => (Logs, a) -> MonadAnim a m ()
appendState (l, a) = do
  index  <- gets currentIndex
  render <- asks renderFun
  assert (index == 0) $ modify $ \s ->
    s { currentIndex = 1
      , stateHistory = take 101 $! (l, render a) : stateHistory s
      , lastState    = a
      }

stepForwards :: Monad m => MonadAnim a m ()
stepForwards = do
  computeNext
  whenM ((> 0) <$> gets currentIndex) $
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
  (l, a) <- liftAnim =<< asks origState
  render <- asks renderFun
  modify $ \s -> s { stateHistory = [(l, render a)]
                   , currentIndex = 0
                   , frameCount   = 0
                   , lastState    = a
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
  clearAndReset
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
  void getChar >> clearScreen

renderFrame :: MonadIO m => MonadAnim a m ()
renderFrame = do
  (logs, a) <- getCurrent
  paused    <- gets isPaused
  delay     <- gets currentDelay
  n         <- gets frameCount
  clear     <- asks clearFun
  liftIO $ do
    clear
    let pl = if paused then " (paused)" else ""
    printf "Interval: %dms%s                 \nFrame: #%d        \n%s" delay pl n a
    forM_ logs $ \l -> clearLine >> putStrLn l
    clearLine

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

animate :: Milliseconds -> ScreenAction -> RenderFun a -> (a -> Maybe (Logs, a)) -> (Logs, a) -> IO ()
animate delay clear render step = animateM delay clear render (return . step) . return

animateM :: MonadIO m => Milliseconds -> ScreenAction -> RenderFun a -> UpdateFun a m -> m (Logs, a) -> m ()
animateM delay clear render step initialState = do
  liftIO $ do
    hSetBuffering stdin NoBuffering
    hSetEcho stdout False
    clearAndReset
  is <- initialState
  let animS = mkAnimState render delay is
      animP = AP render step clear delay initialState
  run `runReaderT` animP `evalStateT` animS
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

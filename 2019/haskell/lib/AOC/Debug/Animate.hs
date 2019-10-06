{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module AOC.Debug.Animate where



-- imports

import           Control.Applicative
import           Control.Concurrent.Lifted
import           Control.Exception.Lifted
import           Control.Monad.Base
import           Control.Monad.Extra         hiding (whileM)
import           Control.Monad.Loops         (whileM)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Control
import           Data.Maybe
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

sleep :: Milliseconds -> IO ()
sleep = threadDelay . (1000 *)

defaultDelay :: Milliseconds
defaultDelay = 1000



-- animation data

type Logs = [String]

type RenderFun a = a -> String
type UpdateFun m a = a -> m (Maybe (Logs, a))

data AnimParams m a = AP { renderFun :: RenderFun a
                         , updateFun :: UpdateFun m a
                         , origDelay :: Int
                         , origState :: (Logs, a)
                         }

data AnimState a = AS { currentDelay :: Milliseconds
                      , stateHistory :: [(Logs, a)]
                      , currentIndex :: Int
                      , isPaused     :: Bool
                      , canContinue  :: Bool
                      , frameCount   :: Int
                      }

type AnimMonad m a = ReaderT (AnimParams m a) (StateT (AnimState a) m)
type AnimIO m a = (MonadBaseControl IO m, MonadIO m, MonadBase m m)



-- animation functions

mkAnimState :: Milliseconds -> (Logs, a) -> AnimState a
mkAnimState d la = AS d [la] 0 False True 0

getCurrent :: Monad m => AnimMonad m a (Logs, a)
getCurrent = liftA2 (!!) hist index
  where hist  = gets stateHistory
        index = gets currentIndex

appendState :: Monad m => (Logs, a) -> AnimMonad m a ()
appendState a = do
  index <- gets currentIndex
  assert (index == 0) $ modify $ \s ->
    s { frameCount   = frameCount s + 1
      , stateHistory = a : take 20 (stateHistory s)
      }

stepForwards :: (MonadBase m m, Monad m) => AnimMonad m a ()
stepForwards = do
  index <- gets currentIndex
  case index of
    0 -> do
      f <- asks updateFun
      a <- snd <$> getCurrent
      r <- liftBase $ f a
      whenJust r appendState
    _ -> modify $ \s ->
      s { frameCount   = frameCount   s + 1
        , currentIndex = currentIndex s - 1
        }

stepBackwards :: Monad m => AnimMonad m a ()
stepBackwards = do
  index <- gets currentIndex
  limit <- length <$> gets stateHistory
  when (index < limit - 1) $ modify $ \s ->
    s { frameCount   = frameCount   s - 1
      , currentIndex = currentIndex s + 1
      }

restart :: Monad m => AnimMonad m a ()
restart = do
  d <- asks origDelay
  a <- asks origState
  put $ mkAnimState d a



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



-- animate

async :: MonadBaseControl IO m => MVar x -> m x -> m ()
async channel action = putMVar channel =<< evaluate =<< action

printHelp :: MonadIO m => AnimMonad m a ()
printHelp = liftIO $ do
  clearScreen >> resetCursor
  putStrLn $ unlines [ "AOC animator deluxe keyboard shortcuts."
                     , "(press any key to continue)"
                     , ""
                     , "<space> pause or resume"
                     , "+       speed up"
                     , "-       speed down"
                     , "=       reset original speed"
                     , "r       restart from original state"
                     , "p       previous state (when paused)"
                     , "n       next state (when paused)"
                     , "?       print this help"
                     , "q       quit"
                     ]
  void getChar

renderFrame :: MonadIO m => AnimMonad m a ()
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
    printf "Delay: %dms%s\nFrame: #%d\n%s%s" delay pl n frameData $ unlines logs

processActions :: AnimIO m a => [Action] -> AnimMonad m a ()
processActions = mapM_ exec
  where
    exec Pause      = modify $ \s -> s { isPaused = not $ isPaused s }
    exec Faster     = modify $ \s -> s { currentDelay = faster $ currentDelay s }
    exec Slower     = modify $ \s -> s { currentDelay = slower $ currentDelay s }
    exec ResetSpeed = do
      value <- asks origDelay
      modify $ \s -> s { currentDelay = value }
    exec Restart    = restart
    exec PrevFrame  = whenM (gets isPaused) $ stepBackwards >> renderFrame
    exec NextFrame  = whenM (gets isPaused) $ stepForwards  >> renderFrame
    exec Help       = printHelp >> renderFrame
    exec Quit       = modify $ \s -> s { canContinue = False }
    faster d = max  100 $ round $ fromIntegral d * 0.80
    slower d = min 2000 $ round $ fromIntegral d * 1.25


animate :: AnimIO m a => Milliseconds -> RenderFun a -> UpdateFun m a -> (Logs, a) -> m ()
animate delay render step initialState = do
  liftIO $ do
    hSetBuffering stdin NoBuffering
    hSetEcho stdout False
  let animS = mkAnimState delay initialState
      animP = AP render step delay initialState
  channel <- liftIO newEmptyMVar
  flip evalStateT animS $ flip runReaderT animP $ run channel
  where
    run :: AnimIO m a => MVar (Maybe (Logs, a)) -> AnimMonad m a ()
    run channel = do
      processActions =<< liftIO peekActions
      renderFrame
      whileM (gets isPaused) $ do
        liftIO $ hWaitForInput stdin (-1)
        processActions =<< liftIO peekActions
      whenM (gets canContinue) $ do
        i <- gets currentIndex
        if i == 0
        then do
          a <- getAction
          liftBase $ fork $ async channel a
          liftIO . sleep =<< gets currentDelay
          liftIO $ whenM (isEmptyMVar channel) $ putStrLn "(waiting...)"
          whenJustM (liftIO $ takeMVar channel) $ \s -> do
            appendState s
            run channel
        else do
          stepForwards
          liftIO . sleep =<< gets currentDelay
          run channel

getAction :: Monad m => AnimMonad m a (m (Maybe (Logs, a)))
getAction = do
  f <- asks updateFun
  a <- snd <$> getCurrent
  return $ f a

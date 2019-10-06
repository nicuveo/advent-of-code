module AOC.Debug.Animate where



-- imports

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Extra        hiding (whileM)
import           Control.Monad.Loops        (whileM)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
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
type UpdateFun a = a -> IO (Maybe (Logs, a))

data AnimParams a = AP { renderFun :: RenderFun a
                       , updateFun :: UpdateFun a
                       , origDelay :: Int
                       , origState :: a
                       }

data AnimState a = AS { currentDelay :: Milliseconds
                      , stateHistory :: [(Logs, a)]
                      , currentIndex :: Int
                      , isPaused     :: Bool
                      , canContinue  :: Bool
                      , frameCount   :: Int
                      }

type AnimMonad a = ReaderT (AnimParams a) (StateT (AnimState a) IO)



-- animation functions

mkAnimState :: Milliseconds -> a -> AnimState a
mkAnimState d a = AS d [([], a)] 0 False True 0

getCurrent :: AnimMonad a (Logs, a)
getCurrent = liftA2 (!!) hist index
  where hist  = gets stateHistory
        index = gets currentIndex

appendState :: (Logs, a) -> AnimMonad a ()
appendState a = do
  index <- gets currentIndex
  assert (index == 0) $ modify $ \s ->
    s { frameCount   = frameCount s + 1
      , stateHistory = a : take 20 (stateHistory s)
      }

stepForwards :: AnimMonad a ()
stepForwards = do
  index <- gets currentIndex
  case index of
    0 -> do
      f <- asks updateFun
      a <- snd <$> getCurrent
      r <- liftIO $ f a
      whenJust r appendState
    _ -> modify $ \s ->
      s { frameCount   = frameCount   s + 1
        , currentIndex = currentIndex s - 1
        }

stepBackwards :: AnimMonad a ()
stepBackwards = do
  index <- gets currentIndex
  limit <- length <$> gets stateHistory
  when (index < limit - 1) $ modify $ \s ->
    s { frameCount   = frameCount   s - 1
      , currentIndex = currentIndex s + 1
      }

restart :: AnimMonad a ()
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

async :: MVar a -> IO a -> IO ()
async channel action = putMVar channel =<< evaluate =<< action

printHelp :: AnimMonad a ()
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

renderFrame :: AnimMonad a ()
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

processActions :: [Action] -> AnimMonad a ()
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

animate :: Milliseconds -> RenderFun a -> UpdateFun a -> a -> IO ()
animate delay render step initialState = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdout False
  let aas = mkAnimState delay initialState
      aap = AP render step delay initialState
  channel <- liftIO newEmptyMVar
  flip evalStateT aas $ flip runReaderT aap $ run channel
  where
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
          f <- asks updateFun
          a <- snd <$> getCurrent
          liftIO $ forkIO $ async channel $ f a
          liftIO . sleep =<< gets currentDelay
          liftIO $ whenM (isEmptyMVar channel) $ putStrLn "(waiting...)"
          whenJustM (liftIO $ takeMVar channel) $ \s -> do
            appendState s
            run channel
        else do
          stepForwards
          liftIO . sleep =<< gets currentDelay
          run channel

{-# LANGUAGE BangPatterns #-}

module AOC.Debug.Animate where



-- imports

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Extra hiding (whileM)
import           Control.Monad.Loops (whileM)
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



-- animation state

data AnimState a = AS { originalDelay :: Milliseconds
                      , currentDelay  :: Milliseconds
                      , originalState :: a
                      , stateHistory  :: [a]
                      , currentIndex  :: Int
                      , isPaused      :: Bool
                      , canContinue   :: Bool
                      , frameCount    :: Int
                      }

mkAnimState :: Milliseconds -> a -> AnimState a
mkAnimState d a = AS d d a [a] 0 False True 0

getCurrent :: AnimState a -> a
getCurrent as = stateHistory as !! currentIndex as

insertState :: AnimState a -> a -> AnimState a
insertState as a = assert (currentIndex as == 0) $
                   as { frameCount   = frameCount as + 1
                      , stateHistory = a : take 20 (stateHistory as)
                      }

stepForwards :: UpdateFun a -> AnimState a -> IO (AnimState a)
stepForwards f s
  | currentIndex s > 0 = return $ s { frameCount   = frameCount   s + 1
                                    , currentIndex = currentIndex s - 1
                                    }
  | otherwise = fmap (maybe s (insertState s)) $ f $ getCurrent s



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

type RenderFun a = a -> String
type UpdateFun a = a -> IO (Maybe a)

async :: MVar a -> IO a -> IO ()
async channel action = putMVar channel =<< evaluate =<< action

noop :: IO ()
noop = return ()

printHelp :: IO ()
printHelp = do
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
  clearScreen >> resetCursor

processActions :: AnimState a -> IO (AnimState a)
processActions animState = peekActions >>= foldM exec animState
  where exec as Faster     = return $ as { currentDelay = faster $ currentDelay as }
        exec as Slower     = return $ as { currentDelay = slower $ currentDelay as }
        exec as ResetSpeed = return $ as { currentDelay = originalDelay as }
        exec as Pause      = return $ as { isPaused     = not $ isPaused as }
        exec as Quit       = return $ as { canContinue  = False }
        exec as Help       = printHelp >> return as
        exec as _          = return as
        faster d = max  100 $ round $ fromIntegral d * 0.80
        slower d = min 2000 $ round $ fromIntegral d * 1.25

animate :: Milliseconds -> RenderFun a -> UpdateFun a -> a -> IO ()
animate delay render step initialState = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdout False
  clearScreen
  channel <- newEmptyMVar
  run channel (0 :: Int) $ mkAnimState delay initialState
  where run channel !n !state = do
          clearScreen >> resetCursor
          newState <- processActions state
          printf "Delay: %dms\nFrame: #%d\n" (currentDelay newState) n
          putStr $ render $ getCurrent newState
          when (canContinue newState) $ do
            forkIO $ async channel $ step $ getCurrent newState
            sleep $ currentDelay newState
            whenM (isEmptyMVar channel) $ putStrLn "(waiting...)"
            whenJustM (takeMVar channel) $ run channel (n+1) . insertState newState

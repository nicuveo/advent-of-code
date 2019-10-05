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
  case c of
    ' ' -> return $ Just Pause
    '+' -> return $ Just Faster
    '-' -> return $ Just Slower
    '=' -> return $ Just ResetSpeed
    'p' -> return $ Just PrevFrame
    'n' -> return $ Just NextFrame
    'q' -> return $ Just Quit
    '?' -> return $ Just Help
    _   -> print [c] >> return Nothing



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

processActions :: Milliseconds -> Milliseconds -> IO (Milliseconds, Bool, Bool)
processActions original current = peekActions >>= foldM exec (current, False, True)
  where exec (d, p, c) Faster     = return (max  100 $ round $ fromIntegral d * 0.80, p, c)
        exec (d, p, c) Slower     = return (min 2000 $ round $ fromIntegral d * 1.25, p, c)
        exec (d, p, c) ResetSpeed = return (original, p, c)
        exec (d, p, c) Quit       = return (d, p, False)
        exec s         Help       = printHelp >> return s
        exec s         _          = return s

animate :: Milliseconds -> RenderFun a -> UpdateFun a -> a -> IO ()
animate originalDelay render step initialState = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdout False
  clearScreen
  channel <- newEmptyMVar
  run originalDelay channel (0 :: Int) initialState
  where run delay channel !n !object = do
          clearScreen >> resetCursor
          (delayMs, _, continue) <- processActions originalDelay delay
          printf "Delay: %dms\nFrame: #%d\n" delayMs n
          putStr $ render object
          when continue $ do
            forkIO $ async channel $ step object
            sleep delayMs
            whenM (isEmptyMVar channel) $ putStrLn "(waiting...)"
            whenJustM (takeMVar channel) $ run delayMs channel $ n+1

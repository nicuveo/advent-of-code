{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AOC.Debug.Animate where



-- imports

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Extra
import           Data.Word
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
newtype Delay = Delay { delayMs :: Milliseconds}
  deriving (Show, Eq, Ord, Num)

sleep :: Delay -> IO ()
sleep = threadDelay . (1000 *) . delayMs

defaultDelay :: Delay
defaultDelay = Delay 1000



-- animate

type RenderFun a = a -> String
type UpdateFun a = a -> IO (Maybe a)

async :: MVar a -> IO a -> IO ()
async channel action = putMVar channel =<< evaluate =<< action

animate :: ScreenAction -> Delay -> RenderFun a -> UpdateFun a -> a -> IO ()
animate clear delay render step initialState = do
  clearScreen
  channel <- newEmptyMVar
  run channel (0 :: Int) initialState
  where run channel !n !object = do
          clear
          printf "Delay: %dms\nFrame: #%d\n" (delayMs delay) n
          putStr $ render object
          forkIO $ async channel $ step object
          sleep delay
          whenM (isEmptyMVar channel) $ putStrLn "(waiting...)"
          whenJustM (takeMVar channel) $ run channel $ n+1

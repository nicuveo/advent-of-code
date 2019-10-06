{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

import           Control.Concurrent
import           Control.Monad.State
import           System.Random

import           AOC.Debug.Animate


type TestMonad m = StateT StdGen m
type IColor = (Int, Int, Int)


getRandom :: Monad m => (StdGen -> (a, StdGen)) -> TestMonad m a
getRandom f = do
  (a,g) <- gets f
  put g
  return a

randomColor :: Monad m => TestMonad m IColor
randomColor = do
  r <- getRandom $ randomR (0, 255)
  g <- getRandom $ randomR (0, 255)
  b <- getRandom $ randomR (0, 255)
  return (r,g,b)

interpolate :: IColor -> IColor -> Int -> IColor
interpolate (r1, g1, b1) (r2, g2, b2) w =
  ( div (r1 * w + r2 * (19 - w)) 19
  , div (g1 * w + g2 * (19 - w)) 19
  , div (b1 * w + b2 * (19 - w)) 19
  )

toRGB :: IColor -> Color
toRGB ( toEnum -> r
      , toEnum -> g
      , toEnum -> b
      ) = RGB r g b

randomPicture :: Monad m => TestMonad m (Logs, String)
randomPicture = do
  rs <- get
  tl <- randomColor
  bl <- randomColor
  tr <- randomColor
  br <- randomColor
  let lineColors = [(interpolate tl bl y, interpolate tr br y) | y <- [0..19]]
      result =  unlines [ concat [ flip bgColor "  " $ toRGB $ interpolate lc rc x
                                 | x <- [0..19]
                                 ]
                        | (lc, rc) <- lineColors
                        ]
  return (["StdGen: '" ++ show rs ++ "'"], result)

getPicture :: Monad m => StdGen -> TestMonad m (Logs, String)
getPicture s = put s >> randomPicture

nextRandomPicture :: String -> TestMonad IO (Maybe (Logs, String))
nextRandomPicture _ = do
  liftIO $ threadDelay 200000
  Just <$> randomPicture

main :: IO ()
main = do
  s <- getStdGen
  flip evalStateT s $ animate defaultDelay id nextRandomPicture $ getPicture s

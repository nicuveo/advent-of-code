{-# LANGUAGE ViewPatterns #-}

import           System.Random

import           AOC.Debug.Animate


randomColor :: IO (Int, Int, Int)
randomColor = do
  r <- getStdRandom (randomR (0, 255))
  g <- getStdRandom (randomR (0, 255))
  b <- getStdRandom (randomR (0, 255))
  return (r,g,b)

interpolate :: (Int, Int, Int) -> (Int, Int, Int) -> Int -> (Int, Int, Int)
interpolate (r1, g1, b1) (r2, g2, b2) w =
  ( div (r1 * w + r2 * (19 - w)) 19
  , div (g1 * w + g2 * (19 - w)) 19
  , div (b1 * w + b2 * (19 - w)) 19
  )

toRGB :: (Int, Int, Int) -> Color
toRGB ( toEnum -> r
      , toEnum -> g
      , toEnum -> b
      ) = RGB r g b

randomPicture :: IO (Logs, String)
randomPicture = do
  rs <- getStdGen
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
  return (["Seed used: " ++ show rs], result)

nextRandomPicture :: String -> IO (Maybe (Logs, String))
nextRandomPicture = const $ Just <$> randomPicture


main :: IO ()
main = do
  (_, p) <- randomPicture
  animate defaultDelay id nextRandomPicture p

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

randomPicture :: IO String
randomPicture = do
  tl <- randomColor
  bl <- randomColor
  tr <- randomColor
  br <- randomColor
  let lineColors = [(interpolate tl bl y, interpolate tr br y) | y <- [0..19]]
  return $ unlines [ concat [ flip bgColor "  " $ toRGB $ interpolate lc rc x
                            | x <- [0..19]
                            ]
                   | (lc, rc) <- lineColors
                   ]

nextRandomPicture :: String -> IO (Maybe String)
nextRandomPicture = const $ Just <$> randomPicture


main :: IO ()
main = randomPicture >>= animate resetCursor defaultDelay id nextRandomPicture

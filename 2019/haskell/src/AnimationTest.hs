import           Codec.Picture.Gif
import           Codec.Picture.Types
import           Data.ByteString     as B (readFile)

import           AOC.Debug


readGif :: FilePath -> IO [String]
readGif f = map toString . either error id . decodeGifImages <$> B.readFile f
  where toString (ImageRGB8 i@(Image w h _)) =
          unlines [ concat [ flip bgColor "  " $ toColor $ pixelAt i x y
                           | x <- [0..w-1]
                           ]
                  | y <- [0..h-1]
                  ]
        toString _ = error "cannot happen"
        toColor (PixelRGB8 r g b) = RGB r g b

main :: IO ()
main = do
  frames <- readGif "nyan-cat.gif"
  let n   = length frames
      u x = return $ Just ([], mod (x+1) n)
      r x = frames !! x
  animate 90 resetCursor r u $ return ([], 0)

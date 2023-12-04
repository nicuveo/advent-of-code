module AOC.Debug.Color where


-- imports

import Data.Ratio
import Data.Word
import Text.Printf


-- color

data Color = RGB Word8 Word8 Word8

instance Show Color where
  show = flip bgColor "  "

fgColor :: Color -> String -> String
fgColor (RGB r g b) = printf "\ESC[38;2;%d;%d;%dm%s\ESC[0m" r g b

bgColor :: Color -> String -> String
bgColor (RGB r g b) = printf "\ESC[48;2;%d;%d;%dm%s\ESC[0m" r g b

red, yellow, green, cyan, blue, magenta, white, black :: Color
red     = RGB 255   0   0
yellow  = RGB 255 255   0
green   = RGB   0 255   0
cyan    = RGB   0 255 255
blue    = RGB   0   0 255
magenta = RGB 255   0 255
white   = RGB 255 255 255
black   = RGB   0   0   0


-- interpolate

type Weight = Ratio Int

interpolate :: Weight -> Color -> Color -> Color
interpolate w (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB r g b
  where r = round $ w * fromIntegral r2 + (1-w) * fromIntegral r1
        g = round $ w * fromIntegral g2 + (1-w) * fromIntegral g1
        b = round $ w * fromIntegral b2 + (1-w) * fromIntegral b1

interpolateN :: [Color] -> Weight -> Color
interpolateN l w = if b == n
                   then last l
                   else interpolate (w * r n - r b) (l !! b) (l !! succ b)
  where n = length l - 1
        b = floor $ w * r n
        r = (% 1)

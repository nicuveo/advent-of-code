{-# LANGUAGE BangPatterns #-}



-- module

module Day21 (day21_1, day21_2) where



-- import

import           Control.Monad
import           Control.Parallel.Strategies
import           Data.List
import           Data.List.Split             (chunksOf)
import           Data.Maybe
import qualified Data.Vector                 as V
import           Text.Parsec

import           Common



-- solution

day21_1 :: Solution
day21_1 input = show $ countTrue $ V.toList $ grid $ zoomN 5  book start
  where book  = parseBook input
        start = parsePattern ".#./..#/###"

day21_2 :: Solution
day21_2 input = show $ countTrue $ V.toList $ grid $ zoomN 18 book start
  where book  = parseBook input
        start = parsePattern ".#./..#/###"



-- types

data Pattern = Pattern { grid :: V.Vector Bool
                       , size :: Int
                       }

type Rule = (Pattern, Pattern)
type Book = [Rule]

instance Show Pattern where
  show p = unlines $ chunksOf (size p) $ V.toList $ V.map bToC $ grid p
    where bToC False = '.'
          bToC True  = '#'

instance Eq Pattern where
  (==) = matches



-- pattern transformations and matching

horzFlip :: Pattern -> Pattern
horzFlip p@(Pattern g s) = p { grid = V.unsafeBackpermute g $ V.fromList indices }
  where range   = [0 .. s -1]
        indices = [ r * s + c
                  | r <- range
                  , c <- reverse range
                  ]

rotate090 :: Pattern -> Pattern
rotate090 p@(Pattern g s) = p { grid = V.unsafeBackpermute g $ V.fromList indices }
  where range   = [0 .. s -1]
        indices = [ r * s + c
                  | c <- reverse range
                  , r <- range
                  ]

rotate180 :: Pattern -> Pattern
rotate180 p@(Pattern g s) = p { grid = V.unsafeBackpermute g $ V.fromList indices }
  where range   = [0 .. s -1]
        indices = [ r * s + c
                  | r <- reverse range
                  , c <- reverse range
                  ]

rotate270 :: Pattern -> Pattern
rotate270 p@(Pattern g s) = p { grid = V.unsafeBackpermute g $ V.fromList indices }
  where range   = [0 .. s -1]
        indices = [ r * s + c
                  | c <- range
                  , r <- reverse range
                  ]

variations :: Pattern -> [Pattern]
variations pn000 = [pn000, pn090, pn180, pn270,
                    pf000, pf090, pf180, pf270]
  where pn090 = rotate090 pn000
        pn180 = rotate180 pn000
        pn270 = rotate270 pn000
        pf000 = horzFlip  pn000
        pf090 = rotate090 pf000
        pf180 = rotate180 pf000
        pf270 = rotate270 pf000

matches :: Pattern -> Pattern -> Bool
p1 `matches` p2 = size p1 == size p2 && grid p1 `elem` map grid (variations p2)



-- patterns splitting and recombining

subSquares :: Pattern -> [Pattern]
subSquares (Pattern g s)
  | s `mod` 2 == 0 = subs 2
  | otherwise      = subs 3
  where cut n is = Pattern (V.unsafeBackpermute g $ V.fromList is) n
        subs n = [ cut n [ y * s + x
                         | y <- [r .. r+n-1]
                         , x <- [c .. c+n-1]
                         ]
                 | r <- [0,n .. s-1]
                 , c <- [0,n .. s-1]
                 ]

combine :: [Pattern] -> Pattern
combine ps = Pattern (V.unsafeBackpermute (V.concat $ grid <$> ps) $ V.fromList is) $ ls * ss
  where ls = head [i | i <- [1..1000], i*i == length ps]
        ss = size $ head ps
        is = [ lr * ls * ss * ss + lc * ss * ss + sr * ss + sc
             | lr <- [0 .. ls-1]
             , sr <- [0 .. ss-1]
             , lc <- [0 .. ls-1]
             , sc <- [0 .. ss-1]
             ]



-- book rules

transform :: Book -> Pattern -> Pattern
transform b !p = fromMaybe (error "day21: rule not found") $ lookup p b

zoom :: Book -> Pattern -> Pattern
zoom b !p = combine $ parMap rseq (transform b) $ subSquares p

zoomN :: Int -> Book -> Pattern -> Pattern
zoomN n b p = foldl' (zoom b ... const) p [1 .. n]



-- parsing

parseBook :: String -> Book
parseBook = map (parseWith line) . lines
  where line = do
          a <- patternParser
          symbol "=>"
          b <- patternParser
          return (a, b)

parsePattern :: String -> Pattern
parsePattern = parseWith patternParser

patternParser :: Parsec String () Pattern
patternParser = do
  spaces
  pLines <- line `sepBy` char '/'
  when (any (/= length pLines) $ length <$> pLines) $ fail "day21: pattern is not a square"
  return $ Pattern (V.fromList $ concat pLines) $ length pLines
  where
    line = many1 (tryAll [pOn, pOff] <?> "pixel (# or .)")
    pOn  = char '#' >> return True
    pOff = char '.' >> return False

{-# LANGUAGE FlexibleContexts #-}



-- module

module Day20 (day20_1, day20_2, animate) where



-- import

import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Monad.ST
import           Data.List                  as L
import           Data.List.Split            (chunksOf)
import qualified Data.Map.Strict            as M
import           Text.Parsec                hiding (State)
import qualified Data.Vector                as V (freeze, toList)
import qualified Data.Vector.Mutable        as V
import           Text.Printf

import           Common



-- solution

day20_1 :: Solution
day20_1 = show . maximum . M.elems . createDistanceMap . parseRE

day20_2 :: Solution
day20_2 = show . length . filter(>=1000) . M.elems . createDistanceMap . parseRE



-- expression

data Expression a = Value a
                  | Product [Expression a]
                  | Sum     [Expression a]

instance Functor Expression where
  fmap f (Value   x ) = Value   $ f x
  fmap f (Product xs) = Product $ map (fmap f) xs
  fmap f (Sum     xs) = Sum     $ map (fmap f) xs


foldExpr :: Eq a => (a -> x -> a) -> a -> Expression x -> [a]
foldExpr f a (Value   m ) = [f a m]
foldExpr f a (Product ms) = nub $ foldl (\as r -> concat $ foldExpr f <$> as <*> [r]) [a] ms
foldExpr f a (Sum     rs) = nub $ foldl (\as r -> as ++ foldExpr f a r)               []  rs

foldExprM :: (Monad m, Eq a) => (a -> x -> m a) -> a -> Expression x -> m [a]
foldExprM f a (Value   m ) = pure <$> f a m
foldExprM f a (Product ms) = nub <$> foldM (\as r -> fmap concat $ sequence $ foldExprM f <$> as <*> [r]) [a] ms
foldExprM f a (Sum     rs) = nub <$> foldM (\as r -> (as ++) <$> foldExprM f a r)                         []  rs



-- regex

type Regex = Expression String

parseRE :: String -> Regex
parseRE = parseWith regex
  where regex   = char '^' *> matches <* char '$'
        match   = Value   <$> many1 (oneOf "NEWS")
        matches = Product <$> many  (match <|> options)
        options = Sum     <$> between (char '(') (char ')') (matches `sepBy` char '|')



-- points

type Point = (Int, Int)
type DistanceMap = M.Map Point Int

nextPoint :: Point -> Char -> Point
nextPoint (y,x) 'N' = (y-1,x)
nextPoint (y,x) 'E' = (y,x+1)
nextPoint (y,x) 'S' = (y+1,x)
nextPoint (y,x) 'W' = (y,x-1)
nextPoint _     c   = error $ "unexpected direction: '" ++ c : "'"

walk :: MonadState DistanceMap m => Point -> String -> m Point
walk = foldM $ \p c -> do
  n <- (M.! p) <$> get
  let np = nextPoint p c
  modify $ M.insertWith min np $ n+1
  return np



-- paths

allPaths :: Regex -> [String]
allPaths = foldExpr (++) ""

longestPath :: Regex -> Int
longestPath = maximum . foldExpr (+) 0 . fmap length

createDistanceMap :: Regex -> DistanceMap
createDistanceMap  r = execState (foldExprM walk startPoint r) initialMap
  where initialMap = M.singleton startPoint 0
        startPoint = (0,0)



-- debug

testREs :: [String]
testREs = [ "^WNE$"
          , "^ENWWW(NEEE|SSE(EE|N))$"
          , "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
          ]

display :: DistanceMap -> Point -> String
display m here = unlines $ map concat $ chunksOf width $ V.toList $ runST $ do
  v <- V.unsafeNew $ width * height
  V.set v "   "
  V.unsafeWrite v (translate here) $ colorizeFG (50,50,50) "[X]"
  forM_ (M.assocs m) $ \(p,d) -> V.modify v (colorizeBG $ colorOf d) $ translate p
  V.freeze v
  where minX   = minimum $ snd <$> M.keys m
        maxX   = maximum $ snd <$> M.keys m
        minY   = minimum $ fst <$> M.keys m
        maxY   = maximum $ fst <$> M.keys m
        width  = 1 + maxX - minX
        height = 1 + maxY - minY
        translate (y,x) = (y - minY) * width + x - minX
        maxD = maximum $ M.elems m
        colorOf d = if d <= div maxD 2
                    then interpolate d (div maxD 2)        (246, 79, 89) (196,113,237)
                    else interpolate (d - div maxD 2) maxD (196,113,237) ( 18,194,233)

type Color = (Int, Int, Int)

colorizeFG :: Color -> String -> String
colorizeFG (r,g,b) = printf "\ESC[38;2;%d;%d;%dm%s\ESC[0m" r g b
colorizeBG :: Color -> String -> String
colorizeBG (r,g,b) = printf "\ESC[48;2;%d;%d;%dm%s\ESC[0m" r g b

interpolate :: Int -> Int -> Color -> Color -> Color
interpolate 0 0 a _ = a
interpolate x d (r0,g0,b0) (r1,g1,b1) =
  ( r0 + div (x * (r1 - r0)) d
  , g0 + div (x * (g1 - g0)) d
  , b0 + div (x * (b1 - b0)) d
  )

animate :: String -> IO ()
animate r = do
  putStr "\ESC[2J"
  evalStateT (void $ foldExprM walkAndDisplay startPoint $ parseRE r) initialMap
  where initialMap = M.singleton startPoint 0
        startPoint = (0,0)
        walkAndDisplay op s = do
          res <- walk op s
          dm  <- get
          liftIO $ do
            putStr "\ESC[;H"
            putStr $ display dm res
          return res

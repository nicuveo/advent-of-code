{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

-- import

import           Control.Arrow    ((&&&))
import           Control.Monad
import           Data.Function    (on)
import qualified Data.IntMap      as M
import qualified Data.IntSet      as S
import           Data.List
import           Data.List.Split  (splitOn)
import           Data.Maybe
import qualified Data.Vector      as V
import           Numeric          (readInt)
import           Text.Parsec      hiding (count)
import           Text.Parsec.Char
import           Text.Printf

import           AOC
import           AOC.Map.Flat



-- edge

data Edge = Edge Int Int
  deriving Eq

instance Show Edge where
  show (Edge a b) = show (a,b)

fromString :: String -> Edge
fromString = (Edge `on` toInt) .> reverse
  where toInt = fst . head . readInt 2 (const True) (fromEnum . (=='#'))

uniqueId :: Edge -> Int
uniqueId (Edge a b) = min a b



-- image

type Image = FlatMap Char

instance Show Image where
  show = displayWith $ const pure

topEdge, rightEdge, bottomEdge, leftEdge :: Image -> Edge
topEdge    i = fromString [i ! Point yMin x | x <- [xMin..xMax]]
  where (Point yMin xMin, Point _ xMax) = bounds i
rightEdge  i = fromString [i ! Point y xMax | y <- [yMin..yMax]]
  where (Point yMin _, Point yMax xMax) = bounds i
bottomEdge i = fromString [i ! Point yMax x | x <- [xMin..xMax]]
  where (Point _ xMin, Point yMax xMax) = bounds i
leftEdge   i = fromString [i ! Point y xMin | y <- [yMin..yMax]]
  where (Point yMin xMin, Point yMax _) = bounds i

imgEdges :: Image -> [Edge]
imgEdges i = [topEdge i, rightEdge i, bottomEdge i, leftEdge i]

trim, flipH, flipV, r90 :: FlatMap a -> FlatMap a
trim i = from2DList [[i ! Point y x | x <- [xMin+1..xMax-1]] | y <- [yMin+1..yMax-1]]
  where (Point yMin xMin, Point yMax xMax) = bounds i
flipH i = from2DList [[i ! Point y x | x <- reverse [xMin..xMax]] | y <- [yMin..yMax]]
  where (Point yMin xMin, Point yMax xMax) = bounds i
flipV i = from2DList [[i ! Point y x | x <- [xMin..xMax]] | y <- reverse [yMin..yMax]]
  where (Point yMin xMin, Point yMax xMax) = bounds i
r90 i = from2DList [[i ! Point y x | y <- reverse [xMin..xMax]] | x <- [yMin..yMax]]
  where (Point yMin xMin, Point yMax xMax) = bounds i

combinations :: FlatMap a -> [FlatMap a]
combinations i = [      i,      j
                 , r90  i, r90  j
                 , r180 i, r180 j
                 , r270 i, r270 j
                 ]
  where j    = flipH i
        r180 = r90 . r90
        r270 = r90 . r180



-- tile

data Tile = Tile { tId  :: Int
                 , tImg :: Image
                 }

type Tiles = [Tile]

mkTile :: Int -> [String] -> Tile
mkTile tid ls = Tile tid $ from2DList ls

($^) :: (Image -> Image) -> Tile -> Tile
f $^ (Tile tid img) = Tile tid $ f img

($@) :: (Image -> [Image]) -> Tile -> [Tile]
f $@ (Tile tid img) = Tile tid <$> f img

($.) :: (Image -> a) -> Tile -> a
f $. t = f $ tImg t



-- input

parseInput :: String -> Tiles
parseInput = map (parseWith tile) . splitOn "\n\n"
  where tile = do
          symbol "Tile "
          tid <- intLiteral
          symbol ":"
          img <- line `sepBy` newline
          pure $ mkTile tid img
        line = many1 $ oneOf ".#"



-- solution

countUniqueEdges :: Tiles -> M.IntMap Int
countUniqueEdges = M.fromListWith (+) . map (,1) . map uniqueId . concatMap (imgEdges $.)


topLeftCorner :: Tiles -> Tile
topLeftCorner tiles = head $ do
  tile <- concatMap (combinations $@) tiles
  guard $ isCorner tile
  pure tile
  where countMap   = countUniqueEdges tiles
        isCorner i = ((countMap M.!) . uniqueId <$> (imgEdges $. i)) == [1,2,2,1]

findLine
  :: (Image -> Edge)
  -> (Image -> Edge)
  -> Tiles
  -> Tile
  -> [Tile]
findLine prevEdge nextEdge tiles start = go start (nextEdge $. start)
  where go tile edge = fromMaybe [tile] $ do
          nextTile <- find (\t -> tId t /= tId tile && hasEdge edge t) tiles
          fixed    <- find (\i -> edge == (prevEdge $. i)) $ combinations $@ nextTile
          pure $ tile : go fixed (nextEdge $. fixed)
        hasEdge e t  = uniqueId e `elem` map uniqueId (imgEdges $. t)

findColumn, findRow :: Tiles -> Tile -> [Tile]
findColumn = findLine topEdge  bottomEdge
findRow    = findLine leftEdge rightEdge

arrange :: Tiles -> [[Tile]]
arrange tiles = map (findRow tiles) $ findColumn tiles $ topLeftCorner tiles

combine :: [[Tile]] -> Image
combine imgs = from2DList $ do
  y <- [0..iH*mH-1]
  let (mapY, imgY) = y `divMod` iW
  pure $ do
    x <- [0..iW*mW-1]
    let (mapX, imgX) = x `divMod` iH
    pure $ imgMap ! Point mapY mapX ! Point imgY imgX
  where imgMap = trim . tImg <$> from2DList imgs
        (iH, iW) = fmapHeight &&& fmapWidth $ imgMap ! Point 0 0
        (mH, mW) = fmapHeight &&& fmapWidth $ imgMap


monster :: (Int, Int, [Point])
monster = (w, h, p)
  where w = 1 + maximum (px <$> p)
        h = 1 + maximum (py <$> p)
        p = do
          (r, l) <- zip [0..] m
          (c, x) <- zip [0..] l
          [Point r c | x == '#']
        m = [ "                  # "
            , "#    ##    ##    ###"
            , " #  #  #  #  #  #   "
            ]

deleteMonsters :: Image -> (Int, Image)
deleteMonsters img = foldl' (flip ($)) (0,img) $ do
  r <- [0..yMax]
  c <- [0..xMax]
  let ps = (Point r c +) <$> mP
  pure $ \(!n,!i) ->
    if all (\p -> i ! p == '#') ps
    then (n+1, updateWith (V.// [(toIndex i p, 'O') | p <- ps]) i)
    else (n,i)
  where xMax = fmapWidth  img - mW
        yMax = fmapHeight img - mH
        (mW, mH, mP) = monster


part1 :: Tiles -> Int
part1 tiles = product $ do
  let countMap = countUniqueEdges tiles
  tile <- tiles
  let counts = (countMap M.!) . uniqueId <$> (imgEdges $. tile)
  guard $ sort counts == [1,1,2,2]
  pure $ tId tile

part2 :: Tiles -> String
part2 = res . snd . fromJust . find ((> 0) . fst) . map deleteMonsters . combinations . combine . arrange
  where res i = show (count '#' $ toList i) ++ "\n" ++ show i



-- main

main :: IO ()
main = aocMain 20 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  putStrLn $ part2 testInput
  putStrLn $ part2 realInput

example :: String
example = "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###\n\nTile 1951:\n#.##...##.\n#.####...#\n.....#..##\n#...######\n.##.#....#\n.###.#####\n###.##.##.\n.###....#.\n..#.#..#.#\n#...##.#..\n\nTile 1171:\n####...##.\n#..##.#..#\n##.#..#.#.\n.###.####.\n..###.####\n.##....##.\n.#...####.\n#.##.####.\n####..#...\n.....##...\n\nTile 1427:\n###.##.#..\n.#..#.##..\n.#.##.#..#\n#.#.#.##.#\n....#...##\n...##..##.\n...#.#####\n.#.####.#.\n..#..###.#\n..##.#..#.\n\nTile 1489:\n##.#.#....\n..##...#..\n.##..##...\n..#...#...\n#####...#.\n#..#.#.#.#\n...#.#.#..\n##.#...##.\n..##.##.##\n###.##.#..\n\nTile 2473:\n#....####.\n#..#.##...\n#.##..#...\n######.#.#\n.#...#.#.#\n.#########\n.###.#..#.\n########.#\n##...##.#.\n..###.#.#.\n\nTile 2971:\n..#.#....#\n#...###...\n#.#.###...\n##.##..#..\n.#####..##\n.#..####.#\n#..#.#..#.\n..####.###\n..#.#.###.\n...#.#.#.#\n\nTile 2729:\n...#.#.#.#\n####.#....\n..#.#.....\n....#..#.#\n.##..##.#.\n.#.####...\n####.#.#..\n##.####...\n##..#.##..\n#.##...##.\n\nTile 3079:\n#.#.#####.\n.#..######\n..#.......\n######....\n####.#..#.\n.#...#.##.\n#.#####.##\n..#.###...\n..#.......\n..#.###..."

module Main where


-- import

import Control.Lens
import Control.Monad.Loops
import Control.Monad.Writer
import Data.HashMap.Strict  (HashMap)
import Data.HashMap.Strict  qualified as M
import Data.HashSet         (HashSet)
import Data.HashSet         qualified as S
import Data.List            qualified as L
import Data.List.Split
import Data.Traversable

import AOC


-- input

type Point3  = (Int, Int, Int)
type Vector3 = Point3
type Side    = (Point3, Vector3)
type Input   = [Point3]

parseInput :: String -> Input
parseInput = map (toPoint . map read . splitOn ",") . lines
  where
    toPoint [x,y,z] = (x,y,z)
    toPoint _       = error "wrong input format"


-- solution

getSides :: Point3 -> [Side]
getSides (x, y, z) =
  [ ((x,y,z),   (1, 0, 0))
  , ((x,y,z),   (0, 1, 0))
  , ((x,y,z),   (0, 0, 1))
  , ((x-1,y,z), (1, 0, 0))
  , ((x,y-1,z), (0, 1, 0))
  , ((x,y,z-1), (0, 0, 1))
  ]

edges :: Point3 -> [(Side, Point3)]
edges p@(x,y,z) = zip (getSides p)
  [ (x+1,y,z)
  , (x,y+1,z)
  , (x,y,z+1)
  , (x-1,y,z)
  , (x,y-1,z)
  , (x,y,z-1)
  ]

freeSides :: [Point3] -> HashSet Side
freeSides = M.keysSet . M.filter (==1) . L.foldl' insert mempty . concatMap getSides
  where
    insert m side = M.insertWith (+) side (1 :: Int) m

boundingBox :: [Point3] -> (Point3, Point3)
boundingBox ps = ((minX-1, minY-1, minZ-1), (maxX+1, maxY+1, maxZ+1))
  where
    minX = minimum $ view _1 <$> ps
    minY = minimum $ view _2 <$> ps
    minZ = minimum $ view _3 <$> ps
    maxX = maximum $ view _1 <$> ps
    maxY = maximum $ view _2 <$> ps
    maxZ = maximum $ view _3 <$> ps

inBounds :: Point3 -> (Point3, Point3) -> Bool
inBounds (x,y,z) ((minX, minY, minZ), (maxX, maxY, maxZ)) =
  x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ

part1 :: Input -> Int
part1 = S.size . freeSides

part2 :: Input -> Int
part2 points = S.size
  $ execWriter
  $ iterateUntilM (L.null . snd) step (S.singleton (fst bb), [fst bb])
  where
    borders = freeSides points
    bb = boundingBox points
    step (seen, currentPoints) = do
      newPoints <- fmap mconcat $ sequence do
        (side, neighbour) <- edges =<< currentPoints
        pure $
          if | neighbour `S.member` seen ->
                 pure S.empty
             | not (neighbour `inBounds` bb) ->
                 pure S.empty
             | side `S.member` borders -> do
                 tell $ S.singleton side
                 pure S.empty
             | otherwise ->
                 pure $ S.singleton neighbour
      pure (S.union seen newPoints, S.toList newPoints)


-- main

main :: IO ()
main = aocMain 18 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5"

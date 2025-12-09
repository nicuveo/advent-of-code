module Day08 where

import "this" Prelude

import Control.Lens   (_1, (^.))
import Data.HashSet   qualified as Set
import Data.List      qualified as L
import Data.Map       qualified as M
import Data.Sequence  qualified as Seq

import AOC


type P3 = (Int, Int, Int)
type Input = [P3]

parseInput :: String -> Input
parseInput = parseLinesWith do
  x <- number
  symbol ","
  y <- number
  symbol ","
  z <- number
  pure (x,y,z)


sqDistance :: P3 -> P3 -> Int
sqDistance (x1, y1, z1) (x2, y2, z2) = sum
  [ (x2 - x1) ^ (2 :: Int)
  , (y2 - y1) ^ (2 :: Int)
  , (z2 - z1) ^ (2 :: Int)
  ]


data Bounds = Bounds
  { minX :: Int
  , maxX :: Int
  , minY :: Int
  , maxY :: Int
  , minZ :: Int
  , maxZ :: Int
  }
  deriving Show

defaultBounds :: Bounds
defaultBounds =
  Bounds maxBound minBound maxBound minBound maxBound minBound

updateBounds :: Bounds -> P3 -> Bounds
updateBounds Bounds {..} (x,y,z) = Bounds
  { minX = min x minX
  , maxX = max x maxX
  , minY = min y minY
  , maxY = max y maxY
  , minZ = min z minZ
  , maxZ = max z maxZ
  }

computeBounds :: [P3] -> Bounds
computeBounds = foldl' updateBounds defaultBounds

computeThresholdWith :: Int -> Bounds -> Int
computeThresholdWith factor Bounds {..} =
  minimum [maxX - minX, maxY - minY, maxZ - minZ] `div` factor


connections
  :: Input
  -> [(P3, P3)]
connections points =
  concatMap snd $ M.toAscList $ M.fromListWith (<>) do
    (point:otherPoints) <- L.tails points
    otherPoint <- otherPoints
    let d = sqDistance point otherPoint
    guard $ d < threshold * threshold
    pure (d, [(point, otherPoint)])
  where
    bounds = computeBounds points
    threshold = computeThresholdWith 2 bounds

connect
  :: Seq (HashSet P3)
  -> (P3, P3)
  -> Seq (HashSet P3)
connect setList (p1, p2) = case (s1, s2) of
  (Nothing, Nothing) ->
    setList Seq.|> Set.fromList [p1, p2]
  (Just i1, Nothing) ->
    Seq.adjust' (Set.insert p2) i1 setList
  (Nothing, Just i2) ->
    Seq.adjust' (Set.insert p1) i2 setList
  (Just i1, Just i2)
   | i1 == i2  -> setList
   | otherwise ->
       let set2 = Seq.index setList i2
       in
         Seq.deleteAt i2 $
         Seq.adjust' (Set.union set2) i1 setList
  where
    s1 = Seq.findIndexL (p1 `Set.member`) setList
    s2 = Seq.findIndexL (p2 `Set.member`) setList


part1 :: Int -> Input -> Int
part1 amount points = connections points
  & take amount
  & foldl' connect Seq.empty
  & fmap Set.size
  & Seq.sortOn negate
  & Seq.take 3
  & product

part2 :: Input -> Int
part2 points = go Seq.empty $ connections points
  where
    amount = length points
    go _ [] = error "ran out of connections"
    go setList ((p1, p2):edges) =
      let newSets = connect setList (p1, p2)
      in case Seq.viewl newSets of
        set Seq.:< Seq.Empty
         | Set.size set == amount -> (p1 ^. _1) * (p2 ^. _1)
        _ ->
          go newSets edges


example :: String
example = "\
  \162,817,812\n\
  \57,618,57\n\
  \906,360,560\n\
  \592,479,940\n\
  \352,342,300\n\
  \466,668,158\n\
  \542,29,236\n\
  \431,825,988\n\
  \739,650,466\n\
  \52,470,668\n\
  \216,146,977\n\
  \819,987,18\n\
  \117,168,530\n\
  \805,96,715\n\
  \346,949,466\n\
  \970,615,88\n\
  \941,993,340\n\
  \862,61,35\n\
  \984,92,344\n\
  \425,690,689"

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1   10 testInput
  print $ part1 1000 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

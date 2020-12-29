{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

-- import

import           Control.Monad
import           Data.Function       (on)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import           Data.List
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC

{-

            _____
           /     \
          /  x y  \
    ,----<   0 2   >----.
   /      \       /      \
  /  x y   \_____/  x y   \
  \  0 1   /     \  1 2   /
   \      /  x y  \      /
    >----<   1 1   >----<
   /      \       /      \
  /  x y   \_____/  x y   \
  \  1 0   /     \  2 1   /
   \      /  x y  \      /
    `----<   2 0   >----'
          \       /
           \_____/

-}



-- helpers (TODO: move to lib)

(^.) :: (a -> a) -> Int -> (a -> a)
(^.) _ 0  = id
(^.) f 1  = f
(^.) f !n = f . (f ^. (n-1))



-- input

type Path  = [Direction]
type Input = [Path]
type Grid  = S.HashSet Point

parseInput :: String -> Input
parseInput = parseLinesWith $ many1 dir
  where dir = tryAll [  W <$ string "w"
                     ,  E <$ string "e"
                     , NW <$ string "nw"
                     , NE <$ string "ne"
                     , SW <$ string "sw"
                     , SE <$ string "se"
                     ]



-- solution

stepVector :: Direction -> Vector
stepVector = \case
  W  -> Point   0 (-1)
  E  -> Point   0   1
  NW -> Point   1 (-1)
  NE -> Point   1   0
  SW -> Point (-1)  0
  SE -> Point (-1)  1
  d  -> error $ "illegal direction: " ++ show d

walk :: Path -> Point
walk = foldl' oneStep (Point 0 0)
  where oneStep p d = p + stepVector d


mkGrid :: Input -> Grid
mkGrid = foldl' flipTile S.empty . map walk
  where flipTile s p
          | p `S.member` s = S.delete p s
          | otherwise      = S.insert p s

neighbourhood :: Point -> [Point]
neighbourhood p = p : do
  s <- [W,E,NW,NE,SW,SE]
  pure $ p + stepVector s

surface :: Grid -> [Point]
surface = concatMap neighbourhood . S.toList

step :: Grid -> Grid
step !g = S.fromList $ filter isBlack $ surface g
  where isBlack p = case (S.member p g, countTrue $ (`S.member` g) <$> tail (neighbourhood p)) of
          (True,  x) -> x == 1 || x == 2
          (False, x) -> x == 2


part1 :: Input -> Int
part1 = length . filter odd . M.elems . foldl' flipTile M.empty . map walk
  where flipTile m p = M.insertWith (+) p 1 m

part2 :: Input -> Int
part2 = S.size . (step ^. 100) . mkGrid



-- main

main :: IO ()
main = aocMain 24 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "sesenwnenenewseeswwswswwnenewsewsw\nneeenesenwnwwswnenewnwwsewnenwseswesw\nseswneswswsenwwnwse\nnwnwneseeswswnenewneswwnewseswneseene\nswweswneswnenwsewnwneneseenw\neesenwseswswnenwswnwnwsewwnwsene\nsewnenenenesenwsewnenwwwse\nwenwwweseeeweswwwnwwe\nwsweesenenewnwwnwsenewsenwwsesesenwne\nneeswseenwwswnwswswnw\nnenwswwsewswnenenewsenwsenwnesesenew\nenewnwewneswsewnwswenweswnenwsenwsw\nsweneswneswneneenwnewenewwneswswnese\nswwesenesewenwneswnwwneseswwne\nenesenwswwswneneswsenwnewswseenwsese\nwnwnesenesenenwwnenwsewesewsesesew\nnenewswnwewswnenesenwnesewesw\neneswnwswnwsenenwnwnwwseeswneewsenese\nneswnwewnwnwseenwseesewsenwsweewe\nwseweeenwnesenwwwswnew"

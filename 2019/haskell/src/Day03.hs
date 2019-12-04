-- import

import qualified Data.Map    as M
import qualified Data.Set    as S
import           Text.Parsec

import           AOC



-- input

type Input = [[Vector]]

parseInput :: String -> Input
parseInput = map parseLine . lines
  where parseLine = parseWith line
        line = fmap concat $ vectors `sepBy` char ','
        vectors = do
          d <- direction
          s <- intLiteral
          return $ replicate s $ directionVector d
        direction = choice [ char 'U' >> return N
                           , char 'R' >> return E
                           , char 'D' >> return S
                           , char 'L' >> return W
                           ]



-- solution

part1 :: Input -> Int
part1 input = S.findMin $ S.map manhattanNorm $ S.intersection s1 s2
  where [s1, s2] = pathSet . tracePath <$> input
        pathSet = S.fromList . tail

part2 :: Input -> Int
part2 input = minimum $ M.elems $ M.intersectionWith (+) m1 m2
  where [m1, m2] = pathMap . tracePath <$> input
        pathMap p = M.fromListWith const $ tail $ zip p [0..]


tracePath :: [Vector] -> [Point]
tracePath = scanl (+) origin



-- main

main :: IO ()
main = aocMain 3 $ \rawInput -> do
  let input = parseInput rawInput
  print $ part1 input
  print $ part2 input

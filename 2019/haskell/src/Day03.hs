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
        line = vector `sepBy` char ','
        vector = do
          d <- direction
          s <- intLiteral
          return $ s .* directionVector d
        direction = choice [ char 'U' >> return N
                           , char 'R' >> return E
                           , char 'D' >> return S
                           , char 'L' >> return W
                           ]



-- common

manhattanDistance :: Point -> Int
manhattanDistance (Point y x) = abs y + abs x

tracePath :: [Vector] -> [Point]
tracePath = foldl line [Point 0 0]
  where line path v =
          let u = signum v
              d = manhattanDistance v
              p = last path
          in  path ++ [p + n .* u | n <- [1 .. d]]



-- part 1

part1 :: Input -> Int
part1 input = S.findMin $ S.map manhattanDistance $ S.intersection s1 s2
  where [w1, w2] = input
        s1 = pathSet $ tracePath w1
        s2 = pathSet $ tracePath w2

pathSet :: [Point] -> S.Set Point
pathSet = S.fromList . tail



-- part 2

part2 :: Input -> Int
part2 input = minimum $ M.elems $ M.intersectionWith (+) m1 m2
  where [w1, w2] = input
        m1 = pathMap $ tracePath w1
        m2 = pathMap $ tracePath w2

pathMap :: [Point] -> M.Map Point Int
pathMap path = M.fromListWith const $ tail $ zip path [0..]



-- main

main :: IO ()
main = aocMain 3 $ \rawInput -> do
  let input = parseInput rawInput
  print $ part1 input
  print $ part2 input

{-# LANGUAGE BangPatterns #-}



-- import

import           Data.List
import           Data.List.Split
import qualified Data.Map        as M

import           AOC             hiding (findPath)



-- input

type Name  = String
type Input = M.Map Name [Name]

parseInput :: String -> Input
parseInput = foldl' addEdge M.empty . lines
  where addEdge m l =
          let [a,b] = splitOn ")" l
          in  M.insertWith (++) a [b] m



-- solution

part1 :: Input -> Int
part1 input = totalOrbits 0 "COM"
  where totalOrbits !depth name =
          depth + sum [ totalOrbits (depth+1) orbit
                      | orbit <- getOrbits name
                      ]
        getOrbits name = M.findWithDefault [] name input

part2 :: Input -> Int
part2 input = length $ concat $ dropCommon [pathToYOU, pathToSAN]
  where pathToYOU  = findPath input "YOU"
        pathToSAN  = findPath input "SAN"
        dropCommon [a:as, b:bs]
          | a == b = dropCommon [as, bs]
        dropCommon ps = ps

findPath :: Input -> Name -> [Name]
findPath input target = head $ fp "COM"
  where fp n
          | n == target = [[]]
          | otherwise   = concat [map (n:) $ fp o | o <- getOrbits n]
        getOrbits name = M.findWithDefault [] name input



-- main

main :: IO ()
main = aocMain 6 $ \rawInput -> do
  let input = parseInput rawInput
  print $ part1 input
  print $ part2 input

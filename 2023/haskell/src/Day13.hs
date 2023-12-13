module Day13 where


-- import

import AOC
import AOC.Grid.Flat
import "this" Prelude

import Data.Bits       (popCount, xor)
import Data.List.Split (splitWhen)


-- input

type Input = [Grid Char]

parseInput :: String -> Input
parseInput = map from2DList . filter (not . null) . splitWhen null . lines


-- solution

type Zipper a = ([a], [a])

moveNext :: Zipper a -> Maybe (Zipper a)
moveNext (before, after) = case after of
  []     -> Nothing
  (x:as) -> Just (x:before, as)


rows :: Grid Char -> [Int]
rows g = map extractRow $ yRange g
  where
    extractRow y = toInt [g ! Point x y | x <- xRange g]
    toInt = foldl' (\a c -> a * 2 + fromEnum (c == '#')) 0

cols :: Grid Char -> [Int]
cols g = map extractCol $ xRange g
  where
    extractCol x = toInt [g ! Point x y | y <- yRange g]
    toInt = foldl' (\a c -> a * 2 + fromEnum (c == '#')) 0


findMirror1 :: [Int] -> Maybe Int
findMirror1 is = go ([], is)
  where
    go z = do
      (before, after) <- moveNext z
      guard $ not $ null after
      if and $ zipWith (==) before after
      then pure $ length before
      else go (before, after)

findMirror2 :: [Int] -> Maybe Int
findMirror2 is = go ([], is)
  where
    go z = do
      (before, after) <- moveNext z
      guard $ not $ null after
      if check False $ zipWith xor before after
      then pure $ length before
      else go (before, after)

    check hasSeenSmudge = \case
      [] -> hasSeenSmudge
      (n:ns)
        | n == 0       -> check hasSeenSmudge ns
        | isPowerOf2 n -> if hasSeenSmudge then False else check True ns
        | otherwise    -> False

    isPowerOf2 n = popCount n == 1

part1 :: Input -> Int
part1 = sum . map mirrorValue
  where
    mirrorValue g = sum
      [ maybe 0 (100*) $ findMirror1 (rows g)
      , fromMaybe 0 $ findMirror1 (cols g)
      ]

part2 :: Input -> Int
part2 = sum . map mirrorValue
  where
    mirrorValue g = sum
      [ maybe 0 (100*) $ findMirror2 (rows g)
      , fromMaybe 0 $ findMirror2 (cols g)
      ]


-- main

example :: String
example = "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#"

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData
  -- putStrLn $ displayWith (const pure) $ realInput !! 8
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

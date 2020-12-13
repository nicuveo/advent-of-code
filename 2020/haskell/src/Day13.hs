{-# LANGUAGE LambdaCase #-}

-- import

import           Control.Monad
import           Data.Function    (on)
import           Data.List
import           Data.Maybe
import           Data.Tuple       (swap)
import           Text.Parsec
import           Text.Parsec.Char

import           AOC



-- input

type Bus   = Maybe Integer
type Input = (Integer, [Bus])

parseInput :: String -> Input
parseInput = parseWith input
  where input = do
          timestamp <- integer
          buses     <- bus `sepBy` symbol ","
          pure (timestamp, buses)
        bus = choice [ Nothing <$  symbol "x"
                     , Just    <$> integer
                     ]
        integer = toInteger <$> intLiteral



-- solution

part1 :: Input -> Integer
part1 (timestamp, buses) =
  uncurry (*) $ minimum [(bus - timestamp `mod` bus, bus) | bus <- busList]
  where busList = catMaybes buses

part2 :: Input -> Integer
part2 (_, buses) = uncurry mod $ foldl1' combine $ do
  (index, Just bus) <- zip [0..] buses
  pure (-index, bus)



-- taken from https://cronokirby.com/posts/2020/12/chinese-remainder-theorem/
-- thank you! :)
bezout :: Integer -> Integer -> (Integer, Integer)
bezout a b | a < b = swap $ bezout b a
bezout _ 0 = (1, 0)
bezout a b =
  let (q, r) = divMod a b
      (x, y) = bezout b r
  in  (y, x - q * y)

combine :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
combine (index1, bus1) (index2, bus2) =
  (b2 * index1 * bus2 + b1 * index2 * bus1, bus1 * bus2)
  where (b1, b2) = bezout bus1 bus2



-- main

main :: IO ()
main = aocMain 13 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "939\n7,13,x,x,59,x,31,19"

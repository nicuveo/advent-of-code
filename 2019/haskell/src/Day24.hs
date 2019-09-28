-- import

import           AOC



-- input

type Input = String

parseInput :: String -> Input
parseInput = parseWith line
  where line = undefined



-- solution

part1 :: Input -> String
part1 = undefined

part2 :: Input -> String
part2 = undefined



-- main

main :: IO ()
main = aocMain 24 $ \rawInput -> do
  let input = parseInput rawInput
  print $ part1 input
  print $ part2 input

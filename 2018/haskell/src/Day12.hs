-- module

module Day12 (day12_1, day12_2) where



-- import

import           Data.Array
import           Data.Foldable
import           Data.List
import qualified Data.Map      as M
import           Data.Maybe
import           Text.Parsec

import           Common



-- solution

day12_1 :: Solution
day12_1 input = show $ sum [pot | (pot, True) <- assocs result]
  where (start, rules) = parseInput input
        result = iterate (step rules) start !! 20


day12_2 :: Solution
day12_2 input = show $ sum [pot + 50000000000 - step | (pot, True) <- assocs result]
  where (start, rules) = parseInput input
        (step, result) = findFixedPoint rules M.empty 0 start

findFixedPoint rules history iteration plants
  | toList plants `M.member` history = (iteration, plants)
  | otherwise = findFixedPoint rules (M.insert (toList plants) iteration history) (iteration + 1) $ step rules plants


testData :: String
testData = "initial state: #..#.#..##......###...###\
           \\n\
           \\n...## => #\
           \\n..#.. => #\
           \\n.#... => #\
           \\n.#.#. => #\
           \\n.#.## => #\
           \\n.##.. => #\
           \\n.#### => #\
           \\n#.#.# => #\
           \\n#.### => #\
           \\n##.#. => #\
           \\n##.## => #\
           \\n###.. => #\
           \\n###.# => #\
           \\n####. => #"



-- helpers

type Plant  = Bool
type Plants = Array Int Plant
type Rule   = ([Plant], Plant)


(!?) :: Plants -> Int -> Bool
ps !? i
  | i < a || i > b = False
  | otherwise      = ps ! i
  where (a,b) = bounds ps


display :: Plants -> String
display ps = [if p then '#' else '.' | p <- toList ps]


step :: [Rule] -> Plants -> Plants
step rules plants = array (minPot, maxPot) newPlants
  where (minPlant,maxPlant) = bounds plants
        clean = dropWhileEnd (not . snd) . dropWhile (not . snd)
        newPlants = clean [ (p, nextState [plants !? x | x <- [p-2..p+2]])
                          | p <- [minPlant - 4..maxPlant + 4]
                          ]
        nextState p = fromMaybe False $ lookup p rules
        minPot = fst $ head newPlants
        maxPot = fst $ last newPlants


parseInput :: String -> (Plants, [Rule])
parseInput = parseWith puzzle
  where puzzle = do
          symbol "initial state: "
          ps <- plants
          spaces
          rules <- rule `sepBy` newline
          return (listArray (0, length ps-1) ps, rules)
        plants = many1 plant
        plant  = do
          c <- oneOf ".#"
          return $ c == '#'
        rule = do
          input <- plants
          symbol "=> "
          output <- plant
          return (input, output)

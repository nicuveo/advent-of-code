-- import

import           Data.List
import qualified Data.Map.Strict as M
import           Data.Ratio
import           Text.Parsec

import           AOC



-- input

type Element  = String
type Recipes  = M.Map Element (Int, [(Int, Element)])

parseInput :: String -> Recipes
parseInput = M.insert "ORE" (1, []) . M.fromList . map parseLine . lines
  where parseLine = parseWith recipe
        recipe = do
          ingredients <- quantity `sepBy` string ","
          symbol "=>"
          (n, element) <- quantity
          return (element, (n, ingredients))
        quantity = do
          q <- intLiteral
          char ' '
          n <- many1 upper
          return (q, n)



-- solution

topSort :: Recipes -> Element -> [Element]
topSort m = flip visit []
  where visit :: Element -> [Element] -> [Element]
        visit x l
          | x `elem` l = l
          | otherwise  = x : foldr (visit . snd) l (snd $ m M.! x)

computeOre :: Int -> Recipes -> [Element] -> Int
computeOre fuel r es = foldl' visit baseMap es M.! "ORE"
  where baseMap = M.singleton "FUEL" fuel
        visit m "ORE" = m
        visit m x =
          let (n, ingredients) = r M.! x
              desiredQuantity  = m M.! x
              required         = ceiling $ desiredQuantity % n
              updatedMap       = M.insert x (required * n - desiredQuantity) m
          in  foldl' (add required) updatedMap ingredients
        add required m' (x, e) = M.insertWith (+) e (x * required) m'

maxAmount :: Recipes -> [Element] -> Int
maxAmount recipes elements = tryProduce (0, 10000000)
  where canProduce n = computeOre n recipes elements < 1000000000000
        tryProduce (a, b)
          | b - a < 2 = maximum [x | x <- [a..b], canProduce x]
          | otherwise =
              let middle    = div (a+b) 2
              in  if canProduce middle
                  then tryProduce (middle, b)
                  else tryProduce (a, middle - 1)



-- main

main :: IO ()
main = aocMain 14 $ \rawInput -> do
  let recipes = parseInput rawInput
      ordered = topSort recipes "FUEL"
  print $ computeOre 1       recipes ordered
  print $ computeOre 2944565 recipes ordered
  print $ computeOre 2944566 recipes ordered
  print $ maxAmount recipes ordered

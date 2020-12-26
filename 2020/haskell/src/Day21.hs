-- import

import           Control.Monad
import           Data.Function       (on)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import           Data.List
import           Data.List.Extra     (sumOn')
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC



-- input

type Ingredient = String
type Allergen   = String
type Food       = ([Ingredient], [Allergen])
type Menu       = [Food]

parseInput :: String -> Menu
parseInput = parseLinesWith food
  where food = (,) <$> many1 ingredient <*> allergens
        ingredient = nameLiteral
        allergens = symbol "(contains" *> commaSeparated nameLiteral <* symbol ")"



-- solution

mkAllergenMap :: Menu -> M.HashMap Allergen [Ingredient]
mkAllergenMap = foldl' addAllergens M.empty
  where addAllergens m (is, as) = foldl' (addAllergen is) m as
        addAllergen is m a = M.insertWith intersect a is m

allIngredients :: Menu -> S.HashSet Ingredient
allIngredients = S.fromList . concatMap fst

mkIngredientCount :: Menu -> M.HashMap Ingredient Int
mkIngredientCount = foldl' addIngredients M.empty
  where addIngredients m (is, _) = foldl' addIngredient m is
        addIngredient m i = M.insertWith (+) i 1 m

solve :: (Hashable a, Eq a, Eq b) => M.HashMap a [b] -> M.HashMap a b
solve = M.fromList . go . M.toList
  where
    go l = case find (\(_, ms) -> length ms == 1) l of
      Just (i, [match]) -> (i, match) : go (mapMaybe (remove match) l)
      _                 -> if null l then [] else error "could not solve"
    remove m (i, ms) = case delete m ms of
      []  -> Nothing
      ms' -> Just (i, ms')



part1 :: Menu -> Int
part1 menu = sumOn' (ingredientCount M.!) goodIngredients
  where dangerousIngredients = S.fromList . concat . M.elems . mkAllergenMap
        goodIngredients = S.toList $ allIngredients menu `S.difference` dangerousIngredients menu
        ingredientCount = mkIngredientCount menu

part2 :: Menu -> String
part2 = intercalate "," . map snd . sort . M.toList . solve . mkAllergenMap



-- main

main :: IO ()
main = aocMain 21 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  putStrLn $ part2 testInput
  putStrLn $ part2 realInput

example :: String
example = "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)"

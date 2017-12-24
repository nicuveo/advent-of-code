-- module

module Day24 (day24_1, day24_2) where



-- import

import           Control.Arrow
import           Data.Function
import           Data.List
import           Text.Parsec

import           Common



-- solution

day24_1 :: Solution
day24_1 = show . maximum . map strength . allBridges . parseComponents


day24_2 :: Solution
day24_2 = show . strength . maximumBy (compare `on` score) . allBridges . parseComponents
  where score = length &&& strength



-- helpers

type Port      = Int
type Component = [Port]
type Bridge    = [Component]

strength :: Bridge -> Int
strength = sum . concat

parseComponents :: String -> [Component]
parseComponents = map (parseWith $ intParser `sepBy` symbol "/") . lines

allBridges :: [Component] -> [Bridge]
allBridges = allBridges_ 0 []
  where allBridges_ port hist comps =
          let extended = [ allBridges_ (head $ comp \\ [port]) (comp : hist) comps
                         | comp <- comps
                         , comp `notElem` hist
                         , port `elem` comp
                         ]
          in  if null extended then [hist] else concat extended

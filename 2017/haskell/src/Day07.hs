-- module

module Day07 (day07_1, day07_2) where



-- import

import           Data.Either.Utils
import           Data.Function
import           Data.List
import           Data.Map.Strict   hiding (null, (\\))
import           Data.Maybe
import           Data.Tree
import           Text.Parsec

import           Common



-- solution

day07_1 :: Solution
day07_1 = plateName . rootLabel . mapToTree . parseTower


day07_2 :: Solution
day07_2 = show . lAnomaly . walk . mapToTree . parseTower
  where walk (Node plate forest)
          | null forest = let w = plateWeight plate in Level w w 0
          | otherwise   = let w = plateWeight plate
                              f = sortOn length $ groupBy ((==) `on` lTotalWeight) $ walk <$> forest
                          in case f of
                               [balanced] -> Level w (w + sum (lTotalWeight <$> balanced)) $ sum (lAnomaly <$> balanced)
                               [[Level p t 0], rest@(Level _ r 0:_)] -> Level w (w + sum (lTotalWeight <$> rest) + r) $ r - t + p
                               _ -> error $ "erroneous group: " ++ show f



-- helpers

data Plate = Plate { plateName   :: String
                   , plateWeight :: Int
                   }

data Level = Level { _lPlateWeight :: Int
                   , lTotalWeight  :: Int
                   , lAnomaly      :: Int
                   }

instance Show Level where
  show (Level p t a) = show (p, t, a)

type TowerTree = Tree Plate
type TowerMap  = Map String (Int, [String])

parseTower :: String -> TowerMap
parseTower input = fromList $ fromRight . parse line "" <$> lines input
  where name   = nameParser
        weigth = between (char '(') (char ')') intParser
        arrow  = spaces >> string "->" >> spaces
        line = do
          n <- name
          spaces
          w <- weigth
          p <- fmap (fromMaybe []) $ optionMaybe $ do
            arrow
            name `sepBy1` string ", "
          return (n, (w, p))

mapToTree :: TowerMap -> TowerTree
mapToTree towerMap = unfoldTree walk root
  where root = head $ keys towerMap \\ (elems towerMap >>= snd)
        walk name = let (weight, forest) = towerMap ! name
                    in (Plate name weight, forest)

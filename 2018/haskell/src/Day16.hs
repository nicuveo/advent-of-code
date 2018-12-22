-- module

module Day16 (day16_1, day16_2) where



-- import

import           Data.List
import qualified Data.Map.Strict as M
import           Text.Parsec     as P
import           Text.Printf

import           Assembly
import           Common



-- solution

day16_1 :: Solution
day16_1 = show . countIf (>= 3) . map countMatches . fst . parseInput
  where countMatches s = countIf (check s) instructions


day16_2 :: Solution
day16_2 input = show (foldl' execute [0,0,0,0] prog) ++ "\n" ++ unlines (show <$> M.assocs instMapping)
  where (samples,prog) = parseInput input
        instMapping = M.fromList $ identify $ foldl' restrict M.empty samples
        restrict m s = M.insertWith intersect (sampleInst s) (M.keys $ M.filter (check s) instructionsNames) m
        identify m
          | M.null m  = []
          | otherwise = let newFound = [(inst, name) | (inst, [name]) <- M.assocs m]
                            cleaned  = M.filter (not . null) $ fmap (\\ map snd newFound) m
                        in  newFound ++ identify cleaned
        execute regs ~[i,a,b,c] = (instructionsNames M.! (instMapping M.! i)) a b c regs :: [Int]



-- parser

data Sample = Sample { sampleBefore :: [Int]
                     , sampleAfter  :: [Int]
                     , sampleInst   ::  Int
                     , sampleArgs   :: [Int]
                     }

instance Show Sample where
  show (Sample b a i v) = printf "%2d(%s): %s -> %s" i (unwords $ show <$> v) (show b) $ show a


check :: Sample -> Instruction [Int] -> Bool
check (Sample i o _ ~[a,b,c]) f = f a b c i == o


parseInput :: String -> ([Sample], [[Int]])
parseInput = parseWith $ do
  s <- samples
  p <- P.count 4 intParser `sepBy` newline
  return (s,p)
  where samples = sample `sepEndBy` many1 newline
        sample  = do
          string "Before: "
          b <- char '[' *> (intParser `sepBy` symbol ",") <* char ']'
          newline
          (i:v) <-P.count 4 intParser
          newline
          string "After:  "
          a <- char '[' *> (intParser `sepBy` symbol ",") <* char ']'
          newline
          return $ Sample b a i v

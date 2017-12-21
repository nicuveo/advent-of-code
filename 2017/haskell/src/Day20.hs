-- module

module Day20 (day20_1, day20_2) where



-- import

import           Control.Monad
import           Data.List
import qualified Data.Vector.Unboxed as V
import           Safe
import           Text.Parsec

import           Common



-- solution

day20_1 :: Solution
day20_1 = show . (elemIndexJust <. minimumOn acceleration) . map parseParticle . lines


day20_2 :: Solution
day20_2 = show . length . stepUntilSafe . map parseParticle . lines
  where stepUntilSafe ps
          | all hasReachedFinalCube ps = ps
          | otherwise = let newPs = map step ps
                        in  stepUntilSafe [p | p <- newPs, not $ any (samePosAs p) $ newPs \\ [p]]



-- particles

type Particle = V.Vector Int


(/!\) :: Particle -> Int -> Int
(/!\) = V.unsafeIndex

pX,pY,pZ,vX,vY,vZ,aX,aY,aZ :: Particle -> Int
pX = (/!\ 0)
pY = (/!\ 1)
pZ = (/!\ 2)
vX = (/!\ 3)
vY = (/!\ 4)
vZ = (/!\ 5)
aX = (/!\ 6)
aY = (/!\ 7)
aZ = (/!\ 8)

step :: Particle -> Particle
step p = p V.// [ (0, pX p + vX p + aX p)
                , (1, pY p + vY p + aY p)
                , (2, pZ p + vZ p + aZ p)
                , (3, vX p + aX p)
                , (4, vY p + aY p)
                , (5, vZ p + aZ p)
                ]

acceleration :: Particle -> Int
acceleration p = abs (aX p) + abs (aY p) + abs (aZ p)

samePosAs :: Particle -> Particle -> Bool
p1 `samePosAs` p2 =
  pX p1 == pX p2 &&
  pY p1 == pY p2 &&
  pZ p1 == pZ p2

hasReachedFinalCube :: Particle -> Bool
hasReachedFinalCube part =
  reached (pX part) (vX part) (aX part) &&
  reached (pY part) (vY part) (aY part) &&
  reached (pZ part) (vZ part) (aZ part)
  where reached p v a = if a /= 0
                        then signum p == signum a
                        else v == 0 || signum p == signum v



-- parsing

parseParticle :: String -> Particle
parseParticle = parseWith part
  where part = do
          l <- vec `sepBy` symbol ","
          when (length l /= 3) $ fail "day20: expecting three vectors"
          return $ V.fromList $ concat l
        vec = do
          spaces >> letter >> string "=<"
          l <- intParser `sepBy` symbol ","
          when (length l /= 3) $ fail "day20: expecting three ints in each vector"
          symbol ">"
          return l

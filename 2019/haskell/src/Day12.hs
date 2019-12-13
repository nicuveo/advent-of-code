{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TemplateHaskell  #-}


-- import

import           Control.Lens
import           Data.List
import           Text.Parsec

import           AOC          hiding (px, py)



-- input

data Moon = Moon { _xPosition :: Int
                 , _yPosition :: Int
                 , _zPosition :: Int
                 , _xVelocity :: Int
                 , _yVelocity :: Int
                 , _zVelocity :: Int
                 } deriving (Show, Eq)
makeLenses ''Moon

type Input = [Moon]

parseInput :: String -> [Moon]
parseInput = map parseLine . lines
  where parseLine = parseWith line
        line = do
          symbol "<"
          x <- coordinate "x"
          symbol ","
          y <- coordinate "y"
          symbol ","
          z <- coordinate "z"
          symbol ">"
          return $ Moon x y z 0 0 0
        coordinate n = do
          symbol n
          char '='
          intLiteral



-- simulation step

applyAll :: a -> [a -> a] -> a
applyAll = foldl' (&)

applyGravity :: [Moon] -> [Moon]
applyGravity moons = map (\m -> applyAll m $ concatMap (computeChange m) moons) moons
  where computeChange m1 m2 =
          [ velocityChange (m1 ^. xPosition) (m2 ^. xPosition) xVelocity
          , velocityChange (m1 ^. yPosition) (m2 ^. yPosition) yVelocity
          , velocityChange (m1 ^. zPosition) (m2 ^. zPosition) zVelocity
          ]
        velocityChange pos1 pos2 vel m
           | pos1 < pos2 = m & vel +~ 1
           | pos1 > pos2 = m & vel -~ 1
           | otherwise   = m

applyVelocity :: [Moon] -> [Moon]
applyVelocity = map $ flip applyAll [ \m -> m & xPosition +~ m ^. xVelocity
                                    , \m -> m & yPosition +~ m ^. yVelocity
                                    , \m -> m & zPosition +~ m ^. zVelocity
                                    ]

timeStep :: [Moon] -> [Moon]
timeStep = applyVelocity . applyGravity



-- extracting data

extractX, extractY, extractZ :: [Moon] -> [Int]
extractX = concatMap $ \(Moon px _  _  vx _  _ ) -> [px, vx]
extractY = concatMap $ \(Moon _  py _  _  vy _ ) -> [py, vy]
extractZ = concatMap $ \(Moon _  _  pz _  _  vz) -> [pz, vz]

extractP, extractV :: [Moon] -> [[Int]]
extractP = map $ \(Moon px py pz _  _  _ ) -> [px, py, pz]
extractV = map $ \(Moon _  _  _  vx vy vz) -> [vx, vy, vz]



-- energy

systemEnergy :: [Moon] -> Int
systemEnergy moons = sum $ zipWith (*) potentialEnergy kineticEnergy
  where potentialEnergy = sum . map abs <$> extractP moons
        kineticEnergy   = sum . map abs <$> extractV moons



-- main

main :: IO ()
main = aocMain 12 $ \rawInput -> do
  let input  = parseInput rawInput
      states = iterate timeStep input

  -- part 1
  print $ systemEnergy $ states !! 1000

  -- part 2
  let x:xs = map extractX states
      y:ys = map extractY states
      z:zs = map extractZ states
      xCycle = x : takeWhile (/= x) xs
      yCycle = y : takeWhile (/= y) ys
      zCycle = z : takeWhile (/= z) zs
      xPeriod = length xCycle
      yPeriod = length yCycle
      zPeriod = length zCycle
  print $ lcm xPeriod $ lcm yPeriod zPeriod

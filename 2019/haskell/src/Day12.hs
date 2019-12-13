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



-- solution

applyAll :: a -> [a -> a] -> a
applyAll = foldl' (&)

applyGravity :: [Moon] -> [Moon]
applyGravity moons = [ applyAll m $ concat [ computeChange m o
                                           | o <- moons
                                           , m /= o
                                           ]
                     | m <- moons
                     ]
  where computeChange m1 m2 = [ velocityChange xPosition xVelocity m1 m2
                              , velocityChange yPosition yVelocity m1 m2
                              , velocityChange zPosition zVelocity m1 m2
                              ]
        velocityChange pos vel m1 m2 m =
          let pos1 = m1 ^. pos
              pos2 = m2 ^. pos
          in if | pos1  < pos2 -> m & vel +~ 1
                | pos1  > pos2 -> m & vel -~ 1
                | otherwise    -> m

applyVelocity :: [Moon] -> [Moon]
applyVelocity = map apply
  where apply moon = applyAll moon [ \m -> m & xPosition +~ m ^. xVelocity
                                   , \m -> m & yPosition +~ m ^. yVelocity
                                   , \m -> m & zPosition +~ m ^. zVelocity
                                   ]

timeStep :: [Moon] -> [Moon]
timeStep = applyVelocity . applyGravity


potentialEnergy :: Moon -> Int
potentialEnergy m = sum $ abs <$> [ m ^. xPosition
                                  , m ^. yPosition
                                  , m ^. zPosition
                                  ]

kineticEnergy :: Moon -> Int
kineticEnergy m = sum $ abs <$> [ m ^. xVelocity
                                , m ^. yVelocity
                                , m ^. zVelocity
                                ]

systemEnergy :: [Moon] -> Int
systemEnergy moons = sum [potentialEnergy m * kineticEnergy m | m <- moons]


extractX :: [Moon] -> [Int]
extractX = concatMap $ \(Moon px _ _ vx _ _) -> [px, vx]

extractY :: [Moon] -> [Int]
extractY = concatMap $ \(Moon _ py _ _ vy _) -> [py, vy]

extractZ :: [Moon] -> [Int]
extractZ = concatMap $ \(Moon _ _ pz _ _ vz) -> [pz, vz]



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

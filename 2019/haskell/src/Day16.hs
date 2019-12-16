{-# LANGUAGE ParallelListComp #-}


-- import

import           Data.Char (digitToInt)

import           AOC



-- input

type Signal = [Int]

parseInput :: String -> Signal
parseInput = map digitToInt



-- solution

digitPattern :: Int -> [Int]
digitPattern = (patterns !!)
  where patterns    = map mkPattern [1..]
        mkPattern n = tail $ cycle $ [0,1,0,-1] >>= replicate n

step :: Signal -> Signal
step s = [f i | i <- [0..] | _ <- s]
  where f i = flip mod 10 $ abs $ sum $ zipWith (*) s $ digitPattern i



-- main

main :: IO ()
main = aocMain 16 $ \rawInput -> do
  let input = parseInput rawInput
      z = iterate step input
  putStrLn $ take 8 $ concatMap show $ z !! 100

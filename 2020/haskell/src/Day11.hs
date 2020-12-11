{-# LANGUAGE BangPatterns #-}

-- import

import           Control.Monad

import           AOC
import           AOC.Debug.Animate
import           AOC.Map.Flat



-- data

data Cell   = Floor | Empty | Taken deriving (Eq)
type Layout = FlatMap Cell

instance Show Cell where
  show Floor = "."
  show Empty = "L"
  show Taken = "#"

display :: Layout -> String
display = displayWith $ const show



-- input

parseInput :: String -> Layout
parseInput = from2DList . map (map toCell) . lines
  where toCell '.' = Floor
        toCell 'L' = Empty
        toCell '#' = Taken
        toCell _   = error "input format error"



-- solution

step1 :: Layout -> Layout
step1 layout = pmap go layout
  where go p state =
          let occupied = count Taken $ eightMapNeighboursOf layout p
          in case state of
            Floor -> Floor
            Empty -> if occupied == 0 then Taken else Empty
            Taken -> if occupied >= 4 then Empty else Taken

step2 :: Layout -> Layout
step2 layout = pmap go layout
  where go p state =
          let occupied = count Taken
                [ getState upLeft    $ p + upLeft
                , getState up        $ p + up
                , getState upRight   $ p + upRight
                , getState right     $ p + right
                , getState downRight $ p + downRight
                , getState down      $ p + down
                , getState downLeft  $ p + downLeft
                , getState left      $ p + left
                ]
          in case state of
            Floor -> Floor
            Empty -> if occupied == 0 then Taken else Empty
            Taken -> if occupied >= 5 then Empty else Taken
        getState d !p = case layout !? p of
          Nothing    -> Empty
          Just Empty -> Empty
          Just Taken -> Taken
          Just Floor -> getState d $ p + d
        upLeft    = Point (-1) (-1)
        up        = Point (-1) ( 0)
        upRight   = Point (-1) ( 1)
        right     = Point ( 0) ( 1)
        downRight = Point ( 1) ( 1)
        down      = Point ( 1) ( 0)
        downLeft  = Point ( 1) (-1)
        left      = Point ( 0) (-1)


part1 :: Layout -> Int
part1 = count Taken . toList . snd . findFixPoint step1

part2 :: Layout -> Int
part2 = count Taken . toList . snd . findFixPoint step2



-- main

main :: IO ()
main = aocMain 11 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

  let mkStep f x = Just ([], f x)
  when False $ animate
    250             -- delay between frames (ms)
    resetCursor     -- redraw operation (just put the cursor back to 0,0)
    display         -- how to render a frame
    (mkStep step2)  -- step function
    ([], realInput) -- (text to display on frame 0, initial state)


example :: String
example = "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"

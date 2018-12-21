-- Hello! This is a heavily commented solution for Day 18.
-- Since this problem is a cellular automaton, i used it to learn
-- about comonads, which i knew almost nothing about.

-- Most of this is based on articles i've found. Here are the ones
-- that helped / influenced me the most:
-- * https://chrispenner.ca/posts/conways-game-of-life.html
-- * https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/cellular-automata
-- * https://bartoszmilewski.com/2017/01/02/comonads/



-- ################################################################
-- PART 0: setup

-- nothing very much special here

{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

module Day18 (day18_1, day18_2) where

import           Control.Comonad
import           Control.Comonad.Store
import           Control.Concurrent
import           Control.DeepSeq
import           Data.Array            as A
import qualified Data.Map              as M
import           Data.MemoCombinators  (integral, pair)

import           Common



-- ################################################################
-- PART 1: acres

-- Let's define our data, before we start thinking of how to represent
-- iterations of our automaton.

data Acre = Open | Tree | Lumberyard | OutOfBounds
  deriving (Eq, Ord)

-- Our automaton is virtually infinite; however, outside of the bounds
-- of our window, there's nothing; and this puzzles requires us to
-- ignore everything that is out of bounds. There would be several
-- ways of doing this (such as using Maybe Acres, or always avoiding
-- being out of bounds), but the easiest solution is simply to have a
-- fourth type of cell: OutOfBounds.

instance Show Acre where
  show Open        = "."
  show Tree        = "|"
  show Lumberyard  = "#"
  show OutOfBounds = error "tried to show out of bounds"

readAcre :: Char -> Acre
readAcre '.' = Open
readAcre '|' = Tree
readAcre '#' = Lumberyard
readAcre c   = error $ "unexpected char '" ++ [c] ++ "'"



-- ################################################################
-- PART 2: 2d positioning

-- Let's then define the container we're gonna use to store our grid
-- after parsing it. I would prefer using an unboxed version, but that
-- would require making our Acre type "unboxable", which is slightly
-- more verbose than i want this code to be.

type Point = (Int, Int)
type Acres = A.Array Point Acre

-- Since we're gonna need them, let's define a few helper functions
-- related to positioning. What are the eight neighbours of a given
-- point?

neighboursOf :: Point -> [Point]
neighboursOf (y,x) = [ (y-1,x-1), (y-1,x), (y-1,x+1)
                     , (y  ,x-1)         , (y  ,x+1)
                     , (y+1,x-1), (y+1,x), (y+1,x+1)
                     ]

-- Likewise: let's define a lookup function on Acre.

lookupAcre :: Acres -> Point -> Acre
lookupAcre as p
  | inRange (bounds as) p = as ! p
  | otherwise             = OutOfBounds

-- We can already write how to parse inputs.
-- We make our land 1-indexed, because why not?

parseAcres :: String -> Acres
parseAcres s =
  array coords $ do -- using the fact that List is a monad
    (y, row)  <- zip [1..] rows
    (x, cell) <- zip [1..] row
    return ((y,x), readAcre cell)
  where rows   = lines s
        height = length rows
        width  = length $ head rows
        coords = ( (     1,     1) -- from top left (1,1)
                 , (height, width) -- to bottom right (h,w)
                 )

-- Finally, let's implement the rules of the game. Given an acre and
-- its neighbours, what is the outcome?
-- (The "count" function comes from Common.)

step :: Acre -> [Acre] -> Acre
step acre neighbours =
  case acre of
    OutOfBounds -> OutOfBounds
    Open        -> if count Tree       neighbours >= 3 then Tree       else Open
    Tree        -> if count Lumberyard neighbours >= 3 then Lumberyard else Tree
    Lumberyard  -> if count Lumberyard neighbours * count Tree neighbours > 0
                   then Lumberyard
                   else Open



-- ################################################################
-- PART 3: the Store comonad

-- Before doing this, the only thing i knew about comonads is that
-- they are "monads with the arrows inverted", and that they're good
-- for cellular automata. If you think of monads about putting values
-- inside a context, comonads are more about extracting values OUT of
-- a context (like from a generator for instance).

-- Their definition is not too disimilar to monads' one, indeed:
--   extract   :: Comonad w => w a -> a
--   duplicate :: Comonad w => w a -> w (w a)
--   extend    :: Comonad w => (w a -> b) -> w a -> w b

-- (the w is because it's an inverted m. really.)
-- [insert joke about Wario and Mario here]

-- So what about the Store comonad? Well it is basically this: we have
-- some abstract collection of Values, indexed by Keys. Or if you
-- prefer this analogy: a big (potentially infinite!) warehouse of
-- Things, stored at a given Position. To construct a Store, we need
-- two things: the lookup function (for a given Position, give me a
-- Value), and a current position.
--   data Store position value = Store (position -> value) position

-- extract is obvious, right?
--   extract (Store l p) = l p

-- to extract other values, we just need to move our little forklift
-- around our warehouse.

-- seek :: pos -> Store pos value -> Store pos value
-- seek (Store l _) x = Store l x
-- peek :: pos -> Store pos value -> value
-- peek = extract ... seek

-- But what about "extend"? Well, if we replace "w" by "Store pos" in
-- the definiton, we get:
--   extend :: (Store pos a -> b) -> Store pos a -> Store pos b

-- So... given a function that to a given position and information
-- about its surroundings associates a new value, we can create a new
-- version of our store that holds the new values. It's kinda like
-- "map", but if along the value we were passing the entire list as an
-- argument, and not on lists, and... yeah, not a great analogy. ^^"

-- But it doesn't actually "store" the information, despite its name;
-- since it knows how to lookup `a` values from a position, and how to
-- transform `a` values to `b` values... It can deal with an infinite
-- amount of positions! Move the forklift, call the function, and
-- voilà!
--   extend f s@(Store _ p) = Store (\x -> f $ seek x s) p

-- So! Let's use it to represent our automaton!

type Grid = Store Point

makeGrid :: Acres -> Grid Acre
makeGrid as = store (lookupAcre as) $ fst $ bounds as

-- We can now easily build an update function, that uses the store
-- comonad properties.

stepAll :: Grid Acre -> Grid Acre
stepAll = extend $ \g -> step (extract g) $ experiment neighboursOf g

-- nice! experiment has this type, here:
--   experiment :: (Point -> [Point]) -> Grid Acre -> [Acres]
-- given a grid and a function that associates several other points to
-- a given point, it looks up the value of all the point associated to
-- the current position of our grid. Here, it returns all the
-- neighbouring acres.

-- Aaaaand, that's enough already!
-- So let's define a quick renderer:

gridTo2DList :: (Int, Int) -> Grid a -> [[a]]
gridTo2DList (h,w) g = [[peek (y,x) g | x <- [1..w]] | y <- [1..h]]

renderGrid :: (Int, Int) -> Grid Acre -> String
renderGrid = unlines . map (show =<<) ... gridTo2DList

-- and let's try it in GHCI!
{- stack ghci

> :l src/Day18.hs
> let acres  = parseAcres testData
> let render = putStr . renderGrid (snd $ bounds acres)

> render $ makeGrid acres
[ascii art here]

> render $ stepAll $ makeGrid acres
[ascii art here]

> render $ stepAll $ stepAll $ makeGrid acres
[ascii art here]

> render $ iterate stepAll (makeGrid acres) !! 6
[VERY VERY SLOW ascii art here]

-}

-- so why is it that slow? well... there's no memoization. So when you
-- query a cell at the sixth iteration, it queries all neighbours at
-- the fifth iteration, which themselves...
-- yup, nicely exponential.

-- what can we do?

-- well, we could try Control.Comonad.Representable.Store, which does
-- memoization in whichever Functor i choose in a very abstract
-- manner... but i can't seem to get it to be compatible with my
-- current stack resolver... :/

-- a simpler solution is to simply use Data.MemoCombinators to memoize
-- a store! Under the hood, MemoCombinators uses Tries to build lazy
-- structures, it's pretty amazing!

-- memoize just updates a store to memoize its function

memoize :: Grid a -> Grid a
memoize g = store (pair integral integral f) s
  where (f, s) = runStore g

-- and update does this after doing a stepAll

update :: Grid Acre -> Grid Acre
update = memoize . stepAll

-- let's test it!



-- ################################################################
-- PART 4: some debugging tools

-- animate clears the screen and displays the current state of our
-- acres every 250 ms. Since evaluating a grid and converting it to a
-- string can be quite slow, i use deepseq to force the string to be
-- fully evaluated before we clear the screen, minimizing the
-- blinking.

-- since this uses update, it is fast!
-- you can test it by running `stack ghci`, loading this file, and:
-- > animate testData

animate :: String -> IO ()
animate input     = run $ makeGrid initial
  where initial   = parseAcres input
        maxbounds = snd $ bounds initial
        tick      = 250000
        run !g = do
          let rendering = renderGrid maxbounds g
          putStr $ deepseq rendering "\ESC[2J"
          putStr rendering
          threadDelay tick
          run $ update g

testData :: String
testData = ".#.#...|#.\n\
           \.....#|##|\n\
           \.|..|...#.\n\
           \..|#.....#\n\
           \#.#|||#|#|\n\
           \...#.||...\n\
           \.|....|...\n\
           \||...#|.#|\n\
           \|.||||..|.\n\
           \...#.|..|.\n"



-- ################################################################
-- PART 4: shall we solve this?

-- let's solve our puzzle!
-- we need to compute the "resource value" of a given grid.

resourceValue :: (Int, Int) -> Grid Acre -> Int
resourceValue maxbounds g = trees * yards
  where acres = concat $ gridTo2DList maxbounds g
        trees = count Tree       acres
        yards = count Lumberyard acres

-- part 1 is simply running ten iterations and computing the resource
-- value.

day18_1 :: Solution
day18_1 input = show $ resourceValue maxbounds result
  where initial   = parseAcres input
        result    = iterate update (makeGrid initial) !! 10
        maxbounds = snd $ bounds initial

-- part 2 requires running 1000000000 iterations, but there got to be
-- an easier solution... And yes: lifted almost directly from several
-- previous days / years: we can find a cycle.

-- findcycle stores a map of when we've encountered a given pattern
-- when we find one again, we simply return a tuple of
-- (the pattern, when we encoutered it, how long the cycle is)

findCycle :: (Int, Int) -> M.Map [Acre] Int -> Int -> Grid Acre -> (Grid Acre, Int, Int)
findCycle maxbounds !history !minute !grid =
  case history M.!? acres of
    Just previous -> (grid, minute, minute - previous)
    Nothing       -> findCycle maxbounds newHist (minute+1) $ update grid
  where acres   = concat $ gridTo2DList maxbounds grid
        newHist = M.insert acres minute history

-- we find a cycle, compute how many more iterations we need before
-- we're at the same grid that the one we're gonna get at step
-- 1000000000, and... voilà !

day18_2 :: Solution
day18_2 input = show $ resourceValue maxbounds finalGrid
  where initial       = parseAcres input
        maxbounds     = snd $ bounds initial
        (g, start, n) = findCycle maxbounds M.empty 0 $ makeGrid initial
        remaining     = mod (1000000000 - start) n
        finalGrid     = iterate update g !! remaining



-- ################################################################
-- PART 5: parting thoughts

-- Positives:
-- * i have built an intuition for comonads in genral, and Store
--   in particular! \o/
-- * the resulting code is terse and simple, and the responsibilities
--   are clearly separate between `neighboursOf`, `step`, and `stepAll`.

-- Negatives:
-- * memory leak and unclear memory usage: given that each new
--   iteration is built on the previous one, we can never fully
--   release the memory of previous grids; running this forever will
--   consume all the available memory.

-- I feel like this strong negative point is only the result of me
-- wanting to run "extend" forever, and that in a lot of more common
-- use cases in which you don't have an infinite amount of iterations,
-- this would be more manageable. An example that was given in one of
-- the articles was the Stream comonad, which allows to process points
-- of a stream while having the context of the rest of the stream
-- (like neighbouring points), to implement stuff like filters. One
-- will not apply an infinite amount of filters, and the memory usage
-- will just scale with the amount of filters, which is reasonable.

-- fun stuff!



-- ################################################################
-- PART 6: but wait there's moar

-- out of curiosity: let's build an "imperative" version, to compare!

updateAcres :: Acres -> Acres
updateAcres !acres = array (bounds acres) $ do
  (p, acre) <- A.assocs acres
  let neighbours = lookupAcre acres <$> neighboursOf p
  return (p, step acre neighbours)

renderAcres :: Acres -> String
renderAcres = unlines . map (show . snd =<<) . groupOn (fst . fst) . assocs

-- hmmmmmmm....  upon testing with `:set +s` in GHCI, this version is
-- faster AND uses less memory... and it doesn't require memoisation,
-- and it does require knowing about comonads... But it's very
-- specific. We can't move our window around to look at another subset
-- of our infinite automaton, or do anything fancy. This is more
-- efficient, but less generic, less expressive.

-- trade-offs, i guess?

-- Anyway, thanks for reading this far!
-- This was fun. :)

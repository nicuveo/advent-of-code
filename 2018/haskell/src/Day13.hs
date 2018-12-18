-- module

module Day13 (day13_1, day13_2) where



-- import

import           Control.Monad
import           Data.Array
import           Data.Function
import           Data.List     as L
import           Data.Set      as S
import           Text.Printf

import           Common



-- solution

day13_1 :: Solution
day13_1 input = printf "%d,%d" x y
  where (mineMap, carts) = parseInput input
        (y,x)            = findCrash mineMap carts


day13_2 :: Solution
day13_2 input = printf "%d,%d" x y
  where (mineMap, carts) = parseInput input
        (y,x)            = findLastCart mineMap carts


testData :: String
testData = "/>-<\\  \n\
           \|   |  \n\
           \| /<+-\\\n\
           \| | | v\n\
           \\\>+</ |\n\
           \  |   ^\n\
           \  \\<->/\n"


-- types

type Position = (Int,Int)

type Map = Array Position Char

data Direction = U | R | D | L deriving (Show, Eq, Enum)
data Turn = TurnLeft | StraightAhead | TurnRight deriving (Show, Eq, Enum)

data Cart = Cart { cartPosition  :: Position
                 , cartDirection :: Direction
                 , cartNextTurn  :: Turn
                 } deriving Show

instance Eq Cart where
  c1 == c2 = cartPosition c1 == cartPosition c2

instance Ord Cart where
  compare = compare `on` cartPosition

type Carts = Set Cart

type Collision = Position



-- moving and direction

move :: Position -> Direction -> Position
move (y,x) U = (y-1,x)
move (y,x) R = (y,x+1)
move (y,x) D = (y+1,x)
move (y,x) L = (y,x-1)

turn :: Direction -> Turn -> Direction
turn d TurnLeft  = toEnum $ mod (fromEnum d + 3) 4
turn d TurnRight = toEnum $ mod (fromEnum d + 1) 4
turn d StraightAhead = d

nextNextTurn :: Turn -> Turn
nextNextTurn t = toEnum $ mod (fromEnum t + 1) 3



-- update

step :: Map -> Cart -> Cart
step mineMap (Cart pos dir nt) = Cart newPosition newDirection newNextTurn
  where newPosition = move pos dir
        (newDirection, newNextTurn) = case mineMap ! newPosition of
          '|'  -> (dir, nt)
          '-'  -> (dir, nt)
          '/'  -> case dir of
                   U -> (R, nt)
                   R -> (U, nt)
                   D -> (L, nt)
                   L -> (D, nt)
          '\\' -> case dir of
                   U -> (L, nt)
                   L -> (U, nt)
                   D -> (R, nt)
                   R -> (D, nt)
          '+'  -> (turn dir nt, nextNextTurn nt)
          _    -> error "unexpected map item"



-- part 1

update1 :: Map -> Carts -> Cart -> Either Collision Carts
update1 mineMap carts cart
  | newCart `S.member` carts = Left  $ cartPosition newCart
  | otherwise                = Right $ S.insert newCart setMinusCart
  where setMinusCart = S.delete cart carts
        newCart      = step mineMap cart

nextFrame1 :: Map -> Carts -> Either Collision Carts
nextFrame1 mineMap carts = foldM (update1 mineMap) carts $ S.toAscList carts

findCrash :: Map -> Carts -> Collision
findCrash mineMap carts = case nextFrame1 mineMap carts of
  Left collision -> collision
  Right newCarts -> findCrash mineMap newCarts



-- part 2

update2 :: Map -> Carts -> Cart -> Carts
update2 mineMap carts cart
  | cart `S.notMember` carts = carts
  | newCart `S.member` carts = S.delete newCart setMinusCart
  | otherwise                = S.insert newCart setMinusCart
  where setMinusCart = S.delete cart carts
        newCart      = step mineMap cart

nextFrame2 :: Map -> Carts -> Carts
nextFrame2 mineMap carts = L.foldl' (update2 mineMap) carts $ S.toAscList carts

findLastCart :: Map -> Carts -> Position
findLastCart mineMap carts
  | S.size newCarts == 1 = cartPosition $ head $ S.elems newCarts
  | otherwise            = findLastCart mineMap newCarts
  where newCarts = nextFrame2 mineMap carts



-- parsing

parseInput :: String -> (Map, Carts)
parseInput input = (mineMap, S.fromList carts)
  where inputLines = lines input
        height = length inputLines
        width  = length $ head inputLines
        mineMap = array ((0,0),(height-1,width-1))
          [ ((y,x), clean c)
          | (y, r) <- zip [0..] inputLines
          , (x, c) <- zip [0..] r
          ]
        clean '>' = '-'
        clean '<' = '-'
        clean '^' = '|'
        clean 'v' = '|'
        clean c   = c
        carts = [ Cart (y,x) (direction c) TurnLeft
                | (y, r) <- zip [0..] inputLines
                , (x, c) <- zip [0..] r
                , c `elem` "><^v"
                ]

direction :: Char -> Direction
direction '>' = R
direction '<' = L
direction '^' = U
direction 'v' = D
direction _   = error "wrong direction"

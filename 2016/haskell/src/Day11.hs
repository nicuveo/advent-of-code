{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}



-- module

module Day11 (day11_1,
              day11_2,
              runTests,
              benchmark) where



-- import

import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Bits
import           Data.Function
import           Data.List                   as L
import qualified Data.Map.Strict             as M
import qualified Data.PQueue.Min             as Q
import qualified Data.Sequence               as S

import           Criterion.Main              as Crit
import           Test.Tasty                  as Test
import           Test.Tasty.QuickCheck       hiding ((.&.))
import           Text.Parsec                 hiding (State)

import           Common



-- solution

day11_1 :: Solution
day11_1 input = show $ length $ tail $ findPathAStar start end
  where start = readInput input
        end   = newState [3,3,3,3,3,3,3,3,3,3,3,0,0,0,0]


day11_2 :: Solution
day11_2 input = show $ length $ tail $ findPathAStar start end
  where start = readInput input
        end   = start
        --end = newState [3,3,3,3,3,3,3,3,3,3,3,3,3,3,3]



-- thing

data Thing = Elevator
           | Chip1
           | Generator1
           | Chip2
           | Generator2
           | Chip3
           | Generator3
           | Chip4
           | Generator4
           | Chip5
           | Generator5
           | Chip6
           | Generator6
           | Chip7
           | Generator7
           deriving (Show, Eq, Ord, Enum, Bounded)

allThings :: [Thing]
allThings = [minBound .. maxBound]



-- state

newtype State = State Int
  deriving (Show, Eq, Ord)

newState :: [Int] -> State
newState l
  | length l == 15 && and [x >= 0 && x < 4 | x <- l] =
      State $ foldr1 (\v a -> 4 * a + v) l
  | otherwise = error "newState: wrong input"

toList :: State -> [Int]
toList state = [getFloor thing state | thing <- allThings]

getFloor :: Thing -> State -> Int
getFloor t (State n) = shiftR n (2 * fromEnum t) .&. 3

getItems :: Int -> State -> [Thing]
getItems f state = [ thing
                   | thing <- delete Elevator allThings
                   , getFloor thing state == f
                   ]

moveUp :: Thing -> State -> State
moveUp thing state@(State n)
  | getFloor thing state == 3 = state
  | otherwise                 = State $ n + 4 ^ fromEnum thing

moveDown :: Thing -> State -> State
moveDown thing state@(State n)
  | getFloor thing state == 0 = state
  | otherwise                 = State $ n - 4 ^ fromEnum thing



-- graph

isValid :: State -> Bool
isValid state = (c1==g1 || c1/=g2 && c1/=g3 && c1/=g4 && c1/=g5 && c1/=g6 && c1/=g7) &&
                (c2==g2 || c2/=g1 && c2/=g3 && c2/=g4 && c2/=g5 && c2/=g6 && c2/=g7) &&
                (c3==g3 || c3/=g1 && c3/=g2 && c3/=g4 && c3/=g5 && c3/=g6 && c3/=g7) &&
                (c4==g4 || c4/=g1 && c4/=g2 && c4/=g3 && c4/=g5 && c4/=g6 && c4/=g7) &&
                (c5==g5 || c5/=g1 && c5/=g2 && c5/=g3 && c5/=g4 && c5/=g6 && c5/=g7) &&
                (c6==g6 || c6/=g1 && c6/=g2 && c6/=g3 && c6/=g4 && c6/=g5 && c6/=g7) &&
                (c7==g7 || c7/=g1 && c7/=g2 && c7/=g3 && c7/=g4 && c7/=g5 && c7/=g6)
  where c1 = getFloor Chip1      state
        g1 = getFloor Generator1 state
        c2 = getFloor Chip2      state
        g2 = getFloor Generator2 state
        c3 = getFloor Chip3      state
        g3 = getFloor Generator3 state
        c4 = getFloor Chip4      state
        g4 = getFloor Generator4 state
        c5 = getFloor Chip5      state
        g5 = getFloor Generator5 state
        c6 = getFloor Chip6      state
        g6 = getFloor Generator6 state
        c7 = getFloor Chip7      state
        g7 = getFloor Generator7 state

getNextStates :: State -> [State]
getNextStates state =
  withStrategy (parList rpar) $ do
    carried <- subsequences items
    let carriedCount = length carried
    guard $ carriedCount == 1 || carriedCount == 2
    transform <- [moveUp, moveDown]
    let next = foldr ($) state $ transform <$> Elevator:carried
    guard $ isOkay next
    return next
  where currentFloor = getFloor Elevator state
        items        = getItems currentFloor state
        isOkay s     = s /= state && isValid s



-- pathfinding

heuristic :: State -> State -> Int
heuristic s1 s2 = div (1 + sum (map abs $ on (zipWith (-)) toList s1 s2)) 2

findPathDijkstra :: State -> State -> [State]
findPathDijkstra start end = findPath_ originalMap $ S.viewl $ S.fromList [start]
  where originalMap = M.fromList [(start, start)]
        construct s seen
          | s == start = [start]
          | otherwise  = s : construct (seen M.! s) seen
        findPath_ _ S.EmptyL = error "findPathDijkstra: no path found"
        findPath_ !seen (current S.:< rest)
          | end == current = reverse $ construct end seen
          | otherwise = findPath_ newSeen $ S.viewl $ rest S.>< S.fromList nexts
          where nexts = withStrategy (parList rseq) [ next
                                                    | next <- getNextStates current
                                                    , next `M.notMember` seen
                                                    ]
                newSeen = M.union seen $ M.fromList $ (,current) <$> nexts

findPathAStar :: State -> State -> [State]
findPathAStar start end = findPath_ originalMap $ Q.fromList [(0,0,start)]
  where originalMap = M.fromList [(start, (0,start))]
        construct s seen
          | s == start = [start]
          | otherwise  = s : construct (snd $ seen M.! s) seen
        findPath_ !seen !queue
          | end == current = reverse $ construct end seen
          | otherwise = findPath_ newSeen $ Q.union rest $ Q.fromList [(cost + 1 + heuristic end next, cost + 1, next) | next <- nexts]
          where ((_,cost,current), rest) = Q.deleteFindMin queue
                nexts = withStrategy (parList rseq) [ next
                                                    | next <- getNextStates current
                                                    , maybe True (cost+1 <) $ fmap fst $ M.lookup next seen
                                                    ]
                newSeen = M.union (M.fromList [(next,(cost+1,current)) | next <- nexts]) seen



-- input

readInput :: String -> State
readInput = newState . parseWith line
  where line = intParser `sepBy` space



-- tests

-- testGroup [...] :: TestTree
-- testCase     "label" $ expected @=? actual

newtype Floor = Floor Int deriving (Show)

instance Arbitrary State where
  arbitrary = State <$> choose(0, 4 ^ 15 - 1)

instance Arbitrary Floor where
  arbitrary = Floor <$> choose(0, 3)

checkStateStability :: TestTree
checkStateStability = testProperty "state is stable" predicate
  where predicate s = newState (toList s) == s

checkItemsAndFloors :: TestTree
checkItemsAndFloors = testProperty "getItems and getFloor are consistent" predicate
  where predicate s (Floor f) = and [getFloor t s == f | t <- getItems f s]

runTests :: IO ()
runTests = Test.defaultMain $ testGroup "Day11" tests
  where tests = [ checkStateStability
                , checkItemsAndFloors
                ]

benchmark :: IO ()
benchmark = Crit.defaultMain [ bench "dijkstra" $ whnf (findPathDijkstra    start) end
                             , bench "astar"    $ whnf (findPathAStar       start) end
                             ]
  where start = newState [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
        end   = newState [2,0,0,2,1,0,0,0,0,0,0,0,0,2,1]

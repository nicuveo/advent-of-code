{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}



-- module

module Day11 (day11_1, day11_2,
              findPath, findPathLength,
              runTests) where



-- import

import           Data.List             as L
import qualified Data.Map.Strict       as M
import           Data.Sequence         as Z (ViewL (..), fromList, viewl, (><))
import qualified Data.Set              as S
import           Test.Tasty
import           Test.Tasty.QuickCheck hiding ((><))
import           Text.Parsec           hiding (State)

import           Common



-- solution

day11_1 :: Solution
day11_1 input = show $ findPathLength start end
  where start = readInput input
        end   = newState [3,3,3,3,3,3,3,3,3,3,3]


day11_2 :: Solution
day11_2 _ = "not implemented yet"



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
           deriving (Show, Eq, Ord, Enum, Bounded)

allThings :: [Thing]
allThings = [minBound .. maxBound]

allChips :: [Thing]
allChips = [Chip1,
            Chip2,
            Chip3,
            Chip4,
            Chip5]

allGenerators :: [Thing]
allGenerators = [Generator1,
                 Generator2,
                 Generator3,
                 Generator4,
                 Generator5]

getGenerator :: Thing -> Thing
getGenerator chip
  | chip `elem` allChips = succ chip
  | otherwise            = error "getGenerator: not a chip"

getOtherGenerators :: Thing -> [Thing]
getOtherGenerators chip
  | chip `elem` allChips = delete (getGenerator chip) allGenerators
  | otherwise            = error "getGenerator: not a chip"



-- state

newtype State = State Int
  deriving (Show, Eq, Ord)

newState :: [Int] -> State
newState l
  | length l == 11 && and [x >= 0 && x < 4 | x <- l] =
      State $ foldr1 (\v a -> 4 * a + v) l
  | otherwise = error "newState: wrong input"

toList :: State -> [Int]
toList state = [getFloor thing state | thing <- allThings]

getFloor :: Thing -> State -> Int
getFloor t (State n) = mod (div n $ 4 ^ index) 4
  where index = fromEnum t

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
isValid state = and $ do
  chip <- allChips
  let chipFloor = getFloor chip state
  return $ getFloor (getGenerator chip) state == chipFloor ||
           chipFloor `notElem` [ getFloor gen state
                               | gen <- getOtherGenerators chip
                               ]

getNextStates :: State -> [State]
getNextStates state = filter isOkay $ do
  transform <- [moveUp, moveDown]
  carried   <- [ sub
               | sub <- subsequences items
               , length sub `elem` [1,2]
               ]
  return $ foldr ($) state $ map transform $ Elevator:carried
  where currentFloor = getFloor Elevator state
        items        = getItems currentFloor state
        isOkay s     = s /= state && isValid s

findPath :: State -> State -> [State]
findPath start end = findPath_ originalMap $ viewl $ Z.fromList [start]
  where originalMap = M.fromList [(start, start)]
        construct s seen
          | s == start = [start]
          | otherwise  = s : construct (seen M.! s) seen
        findPath_ _ EmptyL = error "findPath: no path found!"
        findPath_ !seen !(current :< rest)
          | end == current = reverse $ construct end seen
          | otherwise      = findPath_ newSeen $ viewl $ rest >< Z.fromList nexts
          where nexts = nub [ next
                            | next <- getNextStates current
                            , next `M.notMember` seen
                            ]
                newSeen = M.union seen $ M.fromList $ (,current) <$> nexts

findPathLength :: State -> State -> Int
findPathLength start end = findPath_ originalSet $ Z.viewl $ Z.fromList [(0,start)]
  where originalSet = S.fromList [start]
        findPath_ _ EmptyL = error "findPath: no path found!"
        findPath_ !seen !((stage,current) Z.:< rest)
          | end == current = stage
          | otherwise      = findPath_ newSeen $ Z.viewl $ rest Z.>< Z.fromList ((stage+1,) <$> nexts)
          where nexts = nub [ next
                            | next <- getNextStates current
                            , next `S.notMember` seen
                            ]
                newSeen = S.union seen $ S.fromList nexts



-- input

readInput :: String -> State
readInput = newState . parseWith line
  where line = intParser `sepBy` space



-- tests

-- testGroup [...] :: TestTree
-- testCase     "label" $ expected @=? actual

newtype Floor = Floor Int deriving (Show)

instance Arbitrary State where
  arbitrary = State <$> choose(0, 4 ^ 11 - 1)

instance Arbitrary Floor where
  arbitrary = Floor <$> choose(0, 3)

checkStateStability :: TestTree
checkStateStability = testProperty "state is stable" predicate
  where predicate s = newState (toList s) == s

checkItemsAndFloors :: TestTree
checkItemsAndFloors = testProperty "getItems and getFloor are consistent" predicate
  where predicate s (Floor f) = and [getFloor t s == f | t <- getItems f s]

runTests :: IO ()
runTests = defaultMain $ testGroup "Day11" tests
  where tests = [ checkStateStability
                , checkItemsAndFloors
                ]

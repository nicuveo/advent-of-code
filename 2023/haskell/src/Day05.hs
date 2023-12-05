module Day05 where


-- import

import AOC
import "this" Prelude

import Data.Hashable
import Data.HashMap.Strict qualified as M
import Data.Set            qualified as S
import Text.Parsec


-- input

data Input = Input
  { inputSeeds :: [Int]
  , mappings   :: Mappings
  }
  deriving Show

data Unit
  = Seed
  | Soil
  | Fertilizer
  | Water
  | Light
  | Temperature
  | Humidity
  | Location
  deriving (Show, Eq, Enum, Bounded)

instance Hashable Unit where
  hashWithSalt salt = hashWithSalt salt . fromEnum

data Range = Range
  { rangeIn   :: Int
  , rangeOut  :: Int
  , rangeSize :: Int
  }
  deriving (Show, Eq)

instance Ord Range where
  compare (Range in1 _ _) (Range in2 _ _) = compare in1 in2

type Mapping  = S.Set Range
type Mappings = HashMap (Unit, Unit) Mapping


parseInput :: String -> Input
parseInput = parseWith do
  symbol "seeds:"
  seeds    <- many1 number
  mappings <- many1 mapping
  pure $ Input seeds $ M.fromList mappings
  where
    unit = choice
      [ Seed        <$ try (string "seed")
      , Soil        <$ try (string "soil")
      , Fertilizer  <$ try (string "fertilizer")
      , Water       <$ try (string "water")
      , Light       <$ try (string "light")
      , Temperature <$ try (string "temperature")
      , Humidity    <$ try (string "humidity")
      , Location    <$ try (string "location")
      ]
    mapping = do
      u1 <- unit
      string "-to-"
      u2 <- unit
      spaces
      symbol "map:"
      ranges <- many1 range
      pure ((u1, u2), S.fromList ranges)
    range = do
      rOut  <- number
      rIn   <- number
      rSize <- number
      pure $ Range rIn rOut rSize


-- part 1

convertValue :: Mapping -> Int -> Int
convertValue mapping x = case S.lookupLE (Range x 0 0) mapping of
  Nothing -> x
  Just (Range rIn rOut rSize) ->
    if | x - rIn < rSize -> rOut + x - rIn
       | otherwise       -> x

convert
  :: MonadReader Mappings m
  => Unit
  -> Unit
  -> Int
  -> m Int
convert unitFrom unitTo x = do
  mappings <- ask
  pure $ convertValue (mappings M.! (unitFrom, unitTo)) x

seedToLocation
  :: MonadReader Mappings m
  => Int
  -> m Int
seedToLocation =
      convert Seed        Soil
  >=> convert Soil        Fertilizer
  >=> convert Fertilizer  Water
  >=> convert Water       Light
  >=> convert Light       Temperature
  >=> convert Temperature Humidity
  >=> convert Humidity    Location

part1 :: Input -> Int
part1 (Input seeds mappings) = flip runReader mappings do
  locations <- traverse seedToLocation seeds
  pure $ minimum locations


-- part 2

convertRange :: [Range] -> (Int, Int) -> [(Int, Int)]
convertRange = flip go
  where
    go inputRange = \case
      []     -> [inputRange | snd inputRange > 0]
      (r:rs) ->
        let (inputRange', result) = matchRanges inputRange r
        in  result ++ go inputRange' rs

    matchRanges inputRange@(start, len) outputRange@(Range rIn rOut rSize)
      | len < 0 =
        error "ERROR: negative remaining length"
      | len == 0 =
        (inputRange, [])
      | start + len < rIn =
        (inputRange, [])
      | start < rIn =
        let (inputRange', result) = matchRanges (rIn, len - rIn + start) outputRange
        in  (inputRange', (start, rIn - start) : result)
      | start >= rIn + rSize =
        (inputRange, [])
      | otherwise =
        let advance = min len (rSize - start + rIn)
        in  ( (start + advance, len - advance)
            , [(rOut + start - rIn, advance)]
            )

nubRanges :: [(Int, Int)] -> [(Int, Int)]
nubRanges = go . sortOn fst
  where
    go :: [(Int, Int)] -> [(Int, Int)]
    go []  = []
    go [x] = [x]
    go ((s1, l1):(s2, l2):rs)
      | s2 <= s1 + l1 = go $ (s1, max (s1 + l1) (s2 + l2) - s1) : rs
      | otherwise     = (s1, l1) : go ((s2, l2) : rs)

convertAll
  :: MonadReader Mappings m
  => Unit
  -> Unit
  -> [(Int, Int)]
  -> m [(Int, Int)]
convertAll unitFrom unitTo rs = do
  mappings <- ask
  let mappingList = S.toAscList $ mappings M.! (unitFrom, unitTo)
  pure $ nubRanges $ concatMap (convertRange mappingList) rs

allSeedsToAllLocations
  :: MonadReader Mappings m
  => [(Int, Int)]
  -> m [(Int, Int)]
allSeedsToAllLocations =
      convertAll Seed        Soil
  >=> convertAll Soil        Fertilizer
  >=> convertAll Fertilizer  Water
  >=> convertAll Water       Light
  >=> convertAll Light       Temperature
  >=> convertAll Temperature Humidity
  >=> convertAll Humidity    Location

part2 :: Input -> Int
part2 (Input seeds mappings) = fst
  $ head
  $ flip runReader mappings
  $ allSeedsToAllLocations
  $ makeRanges seeds
  where
    makeRanges :: [Int] -> [(Int, Int)]
    makeRanges = \case
      []       -> []
      [_]      -> error "ERROR: odd number of seeds!"
      (s:l:xs) -> (s,l) : makeRanges xs


-- main

example :: String
example = "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4"

main :: String -> IO ()
main rawData = do
  let testInput = parseInput example
      realInput = parseInput rawData

  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

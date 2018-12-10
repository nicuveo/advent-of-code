-- module

module Day04 (day04_1, day04_2) where



-- import

import           Control.Monad (void)
import           Data.List
import qualified Data.Map      as M
import           Text.Parsec

import           Common



-- solution

day04_1 :: Solution
day04_1 logFile = show $ whichGuard * maxMinute
  where allNights = parseInput logFile
        guardInfo = foldl' addShift M.empty allNights
        addShift m n = M.insertWith (+) (guardId n) (countTrue $ awake n) m
        whichGuard = snd $ maximum $ sort [(m, g) | (g, m) <- M.toList guardInfo]
        nightsPerMinute = transpose [ minutes
                                    | (Night g minutes) <- allNights
                                    , g == whichGuard
                                    ]
        asleepPerMinute = zip (map countTrue nightsPerMinute) [0..]
        maxMinute = snd $ maximum asleepPerMinute

day04_2 :: Solution
day04_2 logFile = show $ whichGuard * whichMinute
  where allNights = parseInput logFile
        guards = nub $ map guardId allNights
        nightsPerMinutePerGuard =
          [ (countTrue nightsPerMinute, guard, minute)
          | guard <- guards
          , (minute, nightsPerMinute) <- zip [0..] $
            transpose [ minutes
                      | (Night g minutes) <- allNights
                      , g == guard
                      ]
          ]
        (_, whichGuard, whichMinute) = maximum nightsPerMinutePerGuard



-- helpers

data Night = Night { guardId :: Int
                   , awake   :: [Bool]
                   } deriving Show


display :: [Bool] -> String
display = map toChar
  where toChar True  = 'X'
        toChar False = '.'

generateMinutes :: Int -> [Int] -> [Bool]
generateMinutes start states =
  replicate start False ++ generateMinutes_ False start states
  where generateMinutes_ state p []    = replicate (60 - p) state
        generateMinutes_ state p (c:r) = replicate (c - p) state ++ generateMinutes_ (not state) c r

parseInput :: String -> [Night]
parseInput = parseWith $ many1 night
  where line p = do
          spaces
          char '['
          _ <- many1 $ noneOf " "
          spaces
          hour   <- intParser
          char ':'
          minute <- intParser
          char ']'
          spaces
          info <- p
          return (hour, minute, info)
        shift = do
          string "Guard #"
          guard <- intParser
          string " begins shift"
          return guard
        fallsAsleep = void $ string "falls asleep"
        wakesUp     = void $ string "wakes up"
        night = do
          (hour, minute, guard) <- line shift
          info <- try (line $ try wakesUp <|> fallsAsleep) `sepEndBy` newline
          let start = if hour == 23 then 0 else minute
          return $ Night guard $ generateMinutes start [m | (_, m, _) <- info]

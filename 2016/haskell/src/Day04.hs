-- module

module Day04 (day04_1, day04_2) where



-- import

import           Control.Monad
import           Data.Char
import           Data.Either.Utils
import           Data.List
import           Text.Parsec

import           Common



-- solution

day04_1 :: Solution
day04_1 input = show $ sum $ do
  line <- lines input
  let (name, checksum, sector) = parseLine line
      top5 = take 5 $ map snd $ sort $ map score $ group $ sort $ join name
  guard $ top5 == checksum
  return sector
  where score s = (-length s, head s)

day04_2 :: Solution
day04_2 input = show $ do
  line <- lines input
  let (name, checksum, sector) = parseLine line
      top5 = take 5 $ map snd $ sort $ map score $ group $ sort $ join name
      realName = unwords $ map (toReal sector) name
  guard $ top5 == checksum
  guard $ "northpole" `isSubsequenceOf` realName
  return (realName, sector)
  where score s = (-length s, head s)
        toReal d s = [ chr $ 97 + (ord c - 97 + d) `mod` 26 | c <- s]



-- helpers

parseLine :: String -> ([String], String, Int)
parseLine = fromRight . parse line ""
  where line = do
          name <- many lower `sepEndBy` char '-'
          sector <- many digit
          checksum <- between (char '[') (char ']') $ many lower
          return (name, checksum, read sector)

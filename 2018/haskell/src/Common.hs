-- module

module Common where



-- import

import           Control.Monad
import           Crypto.Hash.MD5        (hash)
import           Data.ByteString.Base16 (encode)
import           Data.ByteString.Char8  (pack, unpack)
import           Data.Function
import           Data.List
import           Text.Parsec
import           Text.Parsec.String



-- types

type Solution = String -> String



-- parsing helpers

parseWith :: Parser a -> String -> a
parseWith = either (error . show) id ... flip parse ""

symbol :: String -> Parser String
symbol s = spaces >> string s

nameParser :: Parser String
nameParser = spaces >> many1 lower

intParser :: Parser Int
intParser = spaces >> fmap read (liftM2 (:) (char '-') number <|> number)
  where number = many1 digit

tryAll :: [Parser a] -> Parser a
tryAll parsers = foldr1 (<|>) (map try parsers)

betweenBraces :: Parser a -> Parser a
betweenBraces = between (char '{') (char '}')

betweenParens :: Parser a -> Parser a
betweenParens = between (char '(') (char ')')

readInt :: String -> Int
readInt = read



-- list helpers

countTrue :: [Bool] -> Int
countTrue = countIf id

countIf :: (a -> Bool) -> [a] -> Int
countIf = length ... filter

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn = minimumBy . on compare

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn = maximumBy . on compare

groupOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy . on (==)



-- md5

hashMD5 :: String -> String
hashMD5 = hashMD5s 1

hashMD5s :: Int -> String -> String
hashMD5s steps input = unpack $ foldl' (const . encode . hash) (pack input) [1..steps]



-- the blackbird

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) (.) (.)



-- other useful combinators

(<.) :: (b -> a -> c) -> (a -> b) -> a -> c
(<.) = (<*>) . flip

(.>) :: (a -> b -> c) -> (a -> b) -> a -> c
(.>) = (<*>)

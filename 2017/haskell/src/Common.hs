-- module

module Common where



-- import

import           Control.Monad
import           Data.Function
import           Data.List
import           Text.Parsec



-- types

type Solution = String -> String



-- parsing helpers

parseWith :: Parsec String () a -> String -> a
parseWith = either (error . show) id ... flip parse ""

symbol :: String -> Parsec String () String
symbol s = spaces >> string s

nameParser :: Parsec String () String
nameParser = spaces >> many1 lower

intParser :: Parsec String () Int
intParser = spaces >> fmap read (liftM2 (:) (char '-') number <|> number)
  where number = many1 digit

tryAll :: [Parsec String () a] -> Parsec String () a
tryAll parsers = foldr1 (<|>) (map try parsers)

betweenBraces :: Parsec String () a -> Parsec String () a
betweenBraces = between (char '{') (char '}')

betweenParens :: Parsec String () a -> Parsec String () a
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



-- the blackbird

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) (.) (.)



-- other useful combinators

(<.) :: (b -> a -> c) -> (a -> b) -> a -> c
(<.) = (<*>) . flip

(.>) :: (a -> b -> c) -> (a -> b) -> a -> c
(.>) = (<*>)

-- module

module Common where



-- import

import           Text.Parsec



-- types

type Solution = String -> String



-- parsing helpers

parseWith :: Parsec String () a -> String -> a
parseWith = either (error . show) id ... flip parse ""

nameParser :: Parsec String () String
nameParser = many1 lower

intParser :: Parsec String () Int
intParser = fmap read $ many1 $ oneOf "-0123456789"

tryAll :: [Parsec String () a] -> Parsec String () a
tryAll parsers = foldr1 (<|>) (map try parsers) <?> "tryAll"

readInt :: String -> Int
readInt = read



-- the blackbird

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) (.) (.)

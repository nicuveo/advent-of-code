-- module

module AOC.Parsing where



-- import

import           Control.Monad
import           Text.Parsec
import           Text.Parsec.String

import           AOC.Misc



-- parsing

parseWith :: Parser a -> String -> a
parseWith = either (error . show) id ... flip parse ""




-- elements

nameLiteral :: Parser String
nameLiteral = spaces >> many1 lower

intLiteral :: Parser Int
intLiteral = spaces >> fmap read (liftM2 (:) (char '-') number <|> number)
  where number = many1 digit



-- language

symbol :: String -> Parser String
symbol s = spaces >> string s

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

braces :: Parser a -> Parser a
braces = between (char '{') (char '}')

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')



-- combinators

tryAll :: [Parser a] -> Parser a
tryAll parsers = foldr1 (<|>) (map try parsers)

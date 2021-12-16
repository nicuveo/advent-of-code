-- module

module AOC.Parsing where


-- import

import           Text.Parsec
import           Text.Parsec.String

import           AOC.Misc


-- parsing

parseWith :: Parser a -> String -> a
parseWith = either (error . show) id ... flip parse ""

parseLinesWith :: Parser a -> String -> [a]
parseLinesWith p s = parseWith p <$> lines s


-- elements

lexeme :: Parsec String u a -> Parsec String u a
lexeme p = p <* spaces

identifier :: Parser String
identifier = lexeme $ many1 lower

number :: Parser Int
number = lexeme $ choice
  [ char '-' *> fmap negate digits
  , char '+' *> digits
  , digits
  ]
  where
    digits = read <$> many1 digit


-- language

symbol :: String -> Parser String
symbol = lexeme . try . string

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

commaSeparated :: Parser a -> Parser [a]
commaSeparated = (`sepBy` symbol ",")


-- combinators

tryAll :: [Parser a] -> Parser a
tryAll = choice . map try

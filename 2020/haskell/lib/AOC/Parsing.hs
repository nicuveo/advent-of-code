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

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

nameLiteral :: Parser String
nameLiteral = lexeme $ many1 lower

intLiteral :: Parser Int
intLiteral = lexeme $ choice
  [ char '-' >> fmap negate number
  , char '+' >> number
  , number
  ]
  where number = read <$> many1 digit



-- language

symbol :: String -> Parser String
symbol = lexeme . string

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")



-- combinators

tryAll :: [Parser a] -> Parser a
tryAll = choice . map try

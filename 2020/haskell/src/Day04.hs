{-# LANGUAGE LambdaCase #-}


-- import

import           Control.Monad
import           Control.Monad.Validate
import           Data.Char
import           Data.Function          (on)
import qualified Data.HashMap.Strict    as M
import           Data.List
import           Data.List.Split        (splitOn)
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char

import           AOC



-- input

type Passport = M.HashMap String String
type Input    = [Passport]

parseInput :: String -> Input
parseInput = map mkPassport . splitOn "\n\n"
  where mkPassport = M.fromList . parseWith entries
        entries = entry `sepBy` spaces
        entry = do
          key <- many1 lower
          char ':'
          value <- manyTill anyChar (void space <|> eof)
          return (key, value)



-- keys

requiredKeys :: [String]
requiredKeys = [ "byr"
               , "iyr"
               , "eyr"
               , "hgt"
               , "hcl"
               , "ecl"
               , "pid"
               ]



-- solution

part1 :: Input -> Int
part1 passports = countTrue [ all (`M.member` passport) requiredKeys
                            | passport <- passports
                            ]

part2 :: Input -> Int
part2 = countTrue . map isValid
  where isValid :: M.HashMap String String -> Bool
        isValid passport = null $ execValidate $ do
          validateField passport "byr" validateByr
          validateField passport "iyr" validateIyr
          validateField passport "eyr" validateEyr
          validateField passport "hgt" validateHgt
          validateField passport "hcl" validateHcl
          validateField passport "ecl" validateEcl
          validateField passport "pid" validatePid
        validateField
          :: Passport
          -> String -- field
          -> (String -> PassportValidation ()) -- validate function
          -> Validate [String] ()
        validateField passport field fun = do
          case M.lookup field passport of
            Nothing -> dispute [field <> " not found"]
            Just f  -> void $ tolerate $ fun f



-- validation

type PassportValidation a = Validate [String] a

validateInt :: String -> Int -> String -> PassportValidation Int
validateInt field size value = do
  unless (all isDigit value) $
    refute [field <> ": contains invalid characters"]
  unless (length value == size) $
    refute [field <> ": wrong length"]
  return $ read value

validateRange :: String -> Int -> Int -> Int -> PassportValidation ()
validateRange field rMin rMax value = do
  unless (value >= rMin) $
    refute [field <> ": " <> show value <> " less than " <> show rMin]
  unless (value <= rMax) $
    refute [field <> ": " <> show value <> " more than " <> show rMax]


validateByr :: String -> PassportValidation ()
validateByr value = do
  year <- validateInt "byr" 4 value
  validateRange "byr" 1920 2002 year

validateIyr :: String -> PassportValidation ()
validateIyr value = do
  year <- validateInt "iyr" 4 value
  validateRange "iyr" 2010 2020 year

validateEyr :: String -> PassportValidation ()
validateEyr value = do
  year <- validateInt "eyr" 4 value
  validateRange "eyr" 2020 2030 year

validateHgt :: String -> PassportValidation ()
validateHgt value = do
  let parsedHeight = parse heightParser "" value
      heightParser = do
        height <- many digit
        unit   <- string "cm" <|> string "in"
        return (read height, unit)
  (height, unit) <- case parsedHeight of
                     Left err -> refute ["hgt:" <> show err]
                     Right x  -> return x
  case unit of
    "cm" -> validateRange "hgt" 150 193 height
    "in" -> validateRange "hgt"  59  76 height
    _    -> refute ["hgt: wrong unit"]

validateHcl :: String -> PassportValidation ()
validateHcl = \case
  ('#':s) -> do
    unless (all (`elem` "0123456789abcdef") s) $
      refute ["hcl: contains invalid characters"]
    unless (length s == 6) $
      refute ["hcl: wrong length"]
  _       -> refute ["hcl: missing leading #"]


validateEcl :: String -> PassportValidation ()
validateEcl value =
  unless (value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) $
    refute ["ecl: unrecognized colour"]

validatePid :: String -> PassportValidation ()
validatePid = void . validateInt "pid" 9



-- main

main :: IO ()
main = aocMain 4 $ \rawData -> do
  let testInput = parseInput example
      realInput = parseInput rawData
  putStrLn "# Part 1"
  print $ part1 testInput
  print $ part1 realInput
  putStrLn "# Part 2"
  print $ part2 testInput
  print $ part2 realInput

example :: String
example = "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f\n\neyr:2029 ecl:blu cid:129 byr:1989\niyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\nhcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022\n\niyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"

{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (join)
import Data.Coerce (coerce)
import Data.List ((\\))
import Data.Void (Void)
import System.IO (hGetContents, stdin)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L (lexeme)
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void String

data Property
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportID
  | CountryID
  deriving (Eq, Show)

newtype Field = Field (Property, String) deriving (Show)

-- Passports can be valid or invalid
newtype Passport = Passport [Field] deriving (Show)

data Validity = Valid | Invalid deriving (Show, Eq)

pField :: Parser Field
pField = dbg "field" $ do
  key <-
    choice
      [ BirthYear <$ string "byr:",
        IssueYear <$ string "iyr:",
        ExpirationYear <$ string "eyr:",
        Height <$ string "hgt:",
        HairColor <$ string "hcl:",
        EyeColor <$ string "ecl:",
        PassportID <$ string "pid:",
        CountryID <$ string "cid:"
      ]
  val <- many (alphaNumChar <|> char '#')
  return $ Field (key, val)

pFields :: Parser [Field]
pFields = dbg "fields" $ someTill (L.lexeme hspace pField) newline

pPassport :: Parser Passport
pPassport = dbg "passport" $ Passport . join <$> some pFields

pInput :: Parser [Passport]
pInput =
  dbg "input" $
    manyTill (L.lexeme space pPassport) eof

properties :: Passport -> [Property]
properties passport =
  fst
    <$> coerce @Passport @[(Property, String)] passport

requiredProperties :: Passport -> Validity
requiredProperties passport
  | required \\ properties passport == [] = Valid
  | otherwise = Invalid
  where
    required =
      [ BirthYear,
        IssueYear,
        ExpirationYear,
        Height,
        HairColor,
        EyeColor,
        PassportID
      ]

validateField :: Field -> Validity
validateField (Field (key, val)) = case key of
  BirthYear ->
    let year = read @Int val
     in if 1920 <= year && year <= 2002 then Valid else Invalid
  IssueYear ->
    let year = read @Int val
     in if 2010 <= year && year <= 2020 then Valid else Invalid
  ExpirationYear ->
    let year = read @Int val
     in if 2010 <= year && year <= 2030 then Valid else Invalid
  Height ->
    let pHeight :: Parser Validity
        pHeight = dbg "height" $ do
          height <- read @Int <$> some numberChar
          unit <- (string "cm" <|> string "in")
          case unit of
            "cm" ->
              return $ if 150 <= height && height <= 193 then Valid else Invalid
            "in" ->
              return $ if 59 <= height && height <= 76 then Valid else Invalid
            _ -> return Invalid
     in case parseMaybe pHeight val of Just v -> v; Nothing -> Invalid
  HairColor ->
    let pHair :: Parser Validity
        pHair = dbg "hair" $ do
          _ <- char '#'
          hex <- count 6 hexDigitChar
          return Valid
     in case parseMaybe pHair val of Just v -> v; Nothing -> Invalid
  EyeColor ->
    let colors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        pEye :: Parser Validity
        pEye = dbg "eyecolor" $ choice (string <$> colors) >> return Valid
     in case parseMaybe pEye val of Just v -> v; Nothing -> Invalid
  PassportID ->
    let pID :: Parser Validity
        pID = dbg "passportID" $ count 9 numberChar >> return Valid
     in case parseMaybe pID val of Just v -> v; Nothing -> Invalid
  CountryID -> Valid

validatePassportFields :: Passport -> Validity
validatePassportFields passport
  | all (== Valid) $ validateField <$> fields = Valid
  | otherwise = Invalid
  where
    fields = coerce @Passport @[Field] passport

validate :: Passport -> Validity
validate passport
  | all (== Valid) $ checks <*> pure passport = Valid
  | otherwise = Invalid
  where
    checks :: [Passport -> Validity]
    checks =
      [requiredProperties, validatePassportFields]

main :: IO ()
main = do
  input <- hGetContents stdin
  case parseMaybe pInput input of
    Nothing -> error "could not parse!"
    Just passports ->
      print . length . filter (== Valid) $ validate <$> passports

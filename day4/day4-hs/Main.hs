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

requiredProperties :: [Property]
requiredProperties =
  [ BirthYear,
    IssueYear,
    ExpirationYear,
    Height,
    HairColor,
    EyeColor,
    PassportID
  ]

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
    <$> coerce @Passport @([(Property, String)]) passport

validate :: Passport -> Validity
validate passport
  | requiredProperties \\ properties passport == [] = Valid
  | otherwise = Invalid

main :: IO ()
main = do
  input <- hGetContents stdin
  case parseMaybe pInput input of
    Nothing -> error "foo"
    Just passports ->
      print . length . filter (== Valid) $ validate <$> passports

module Main where

import Control.Monad (join)
import Data.Coerce (coerce)
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
pPassport = dbg "passport" $ Passport <$> (L.lexeme space pFields)

pInput :: Parser [Passport]
pInput =
  dbg "input" $
    manyTill pPassport eof

main :: IO ()
main = hGetContents stdin >>= parseTest pInput

module Main where

-- import qualified Data.Map.Strict as M

import Data.Coerce (coerce)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L (lexeme)
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void String

newtype Question = Question Char deriving (Show, Eq)

newtype Response = Response [Question] deriving (Show)

pQuestion :: Parser Question
pQuestion = dbg "question" $ coerce <$> letterChar

pResponse :: Parser Response
pResponse = dbg "response" $ coerce <$> manyTill pQuestion eol

main :: IO ()
main = getContents >>= parseTest pResponse

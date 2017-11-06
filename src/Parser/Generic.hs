module Parser.Generic
  ( Parser
  , lexeme
  , sc
  , stringLiteral
  , parseErrorPretty
  ) where

import           Control.Applicative        (empty)
import           Data.Text                  (Text, pack)
import           Data.Void                  (Void)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

stringLiteral :: Parser Text
stringLiteral = pack <$> manyTill L.charLiteral (char ' ')

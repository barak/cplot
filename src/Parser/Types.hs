module Parser.Types
  ( Parser
  , lexeme
  ) where

import           Control.Applicative
import           Data.Text                  (Text)
import           Data.Void                  (Void)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

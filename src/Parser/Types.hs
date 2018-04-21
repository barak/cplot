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

-- | Defines how our parsers will consume space characters. You can use the
--   second and third arguments to define consumers for line/block comments,
--   but we're not parsing a full language here, so we don't need them.
sc :: Parser ()
sc = L.space space1 empty empty

-- | Used to wrap parsers in our space consumer so they will automatically
--   consume that space without explicitly needing to state it.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

module Parser
  ( point
  , parseErrorPretty
  ) where

import           Control.Applicative        (empty)
import           Data.Scientific
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

signed :: Parser Scientific
signed = lexeme (L.signed sc L.scientific)

pointP :: Parser (Double, Double)
pointP = p <$> signed <*> signed
  where p a b = (toRealFloat a, toRealFloat b)

point :: Text -> Either (ParseError (Token Text) Void) (Double, Double)
point = parse pointP ""

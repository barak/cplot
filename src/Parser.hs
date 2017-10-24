module Parser
  ( point
  , parseErrorPretty
  ) where

import           Control.Applicative        (empty)
import           Data.Void                  (Void)

import           Data.Scientific

import qualified Data.ByteString.Char8      as B

import           Text.Megaparsec
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L


type Parser = Parsec Void B.ByteString

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

signed :: Parser Scientific
signed = lexeme (L.signed sc L.scientific)

pointP :: Parser (Double, Double)
pointP = p <$> signed <*> signed
  where p a b = (toRealFloat a, toRealFloat b)

point :: B.ByteString -> Either (ParseError (Token B.ByteString) Void) (Double, Double)
point = parse pointP ""

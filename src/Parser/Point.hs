module Parser.Point
  ( point
  ) where

import           Data.Scientific
import           Data.Text                  (Text)
import           Data.Void                  (Void)

import           Parser.Generic
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

signed :: Parser Scientific
signed = lexeme (L.signed sc L.scientific)

pointP :: Parser (Double, Double)
pointP = p <$> signed <*> signed
  where p a b = (toRealFloat a, toRealFloat b)

point :: Text -> Either (ParseError (Token Text) Void) (Double, Double)
point = parse pointP ""

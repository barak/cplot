module Parser.GUI
  ( parseNumericEntry
  , parseErrorPretty
  ) where

import           Data.Text                  (Text)
import           Data.Void                  (Void)

import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import           Parser.Types


numericEntry :: Parser Int
numericEntry = L.decimal

parseNumericEntry :: Text -> Either (ParseError (Token Text) Void) Int
parseNumericEntry = parse numericEntry ""

module Parser.GUI
  ( parseNumericEntry
  , parseErrorPretty
  ) where

import           Data.Text                  (Text)
import           Data.Void                  (Void)

import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import           Parser.Types

-- | Parser for a single base 10 integer.
numericEntry :: Parser Int
numericEntry = L.decimal

-- | Run the numericEntry parser on some text.
parseNumericEntry :: Text -> Either (ParseError (Token Text) Void) Int
parseNumericEntry = parse numericEntry ""

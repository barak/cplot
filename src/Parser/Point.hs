{-# LANGUAGE TemplateHaskell #-}

module Parser.Point
  ( Message
  , parseMessage

  -- Message lenses
  , chartID
  , point
  ) where

import           Control.Lens
import           Data.Scientific
import           Data.Text                  (Text)
import           Data.Void                  (Void)

import           Parser.Generic
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Chart

data Message = Message
  { _chartID  :: Text
  , _point :: Point
  }

makeLenses ''Message

signed :: Parser Scientific
signed = lexeme (L.signed sc L.scientific)

pointP :: Parser Point
pointP = p <$> signed <*> signed
  where p a b = (toRealFloat a, toRealFloat b)

message :: Parser Message
message = Message <$> stringLiteral
                  <*  lexeme (char ':')
                  <*> pointP

parseMessage :: Text -> Either (ParseError (Token Text) Void) Message
parseMessage = parse message ""

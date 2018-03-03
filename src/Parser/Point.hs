{-# LANGUAGE TemplateHaskell #-}

module Parser.Point
  ( Message
  , parseMessage

  -- Message lenses
  , chartID
  , point
  ) where

import           Control.Lens
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)

import           Chart


data Message = Message
  { _chartID :: ByteString
  , _point   :: Point
  }

makeLenses ''Message

stringLiteral :: Parser ByteString
stringLiteral = takeWhile1 isAlpha_ascii

pointP :: Parser Point
pointP = (,) <$> double
             <*  skipSpace
             <*> double

message :: Parser Message
message = Message <$> stringLiteral
                  <*  char ':'
                  <*  skipSpace
                  <*> pointP

parseMessage :: ByteString -> Either String Message
parseMessage = parseOnly (message <* endOfInput)

{-# LANGUAGE TemplateHaskell #-}

module Parser.Message
  ( Message
  , message
  , parseMessage

  -- Message lenses
  , chartID
  , point
  ) where

import           Control.Applicative              ((<|>))
import           Control.Lens
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import           Dataset.Internal.Types           (Point (..))

data Message = Message
  { _chartID :: ByteString
  , _point   :: Point
  }

makeLenses ''Message

stringLiteral :: Parser ByteString
stringLiteral = takeWhile1 (\c -> isAlpha_ascii c || isDigit c)

-- inefficient, but works for now
pointP :: Parser Point
pointP = (one <* endOfInput) <|> (two <* endOfInput)
  where
    two = P2 <$> double
             <*  skipSpace
             <*> double
    one = P1 <$> double

message :: Parser Message
message = Message <$> stringLiteral
                  <*  char ':'
                  <*  skipSpace
                  <*> pointP

parseMessage :: ByteString -> Either String Message
parseMessage = parseOnly message

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
import           Dataset.Internal.Types           (Point (..))


data Message = Message
  { _chartID :: ByteString
  , _point   :: Point
  }

makeLenses ''Message

stringLiteral :: Parser ByteString
stringLiteral = takeWhile1 (\c -> isAlpha_ascii c || isDigit c)

-- this only parses to the second constructor right now
pointP :: Parser Point
pointP = Point <$> double
               <*  skipSpace
               <*> double

message :: Parser Message
message = Message <$> stringLiteral
                  <*  char ':'
                  <*  skipSpace
                  <*> pointP

parseMessage :: ByteString -> Either String Message
parseMessage = parseOnly (message <* endOfInput)

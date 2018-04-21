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

-- | Each message recieved by cplot is packaged into this data type.
data Message = Message
  { _chartID :: ByteString
  -- ^ Chart identifier. Really it's just the name of the chart.
  , _point   :: Point
  -- ^ The point to send to that chart.
  }

makeLenses ''Message

-- | Parser for a single string literal. Accepts [a-zA-Z0-9].
stringLiteral :: Parser ByteString
stringLiteral = takeWhile1 (\c -> isAlpha_ascii c || isDigit c)

-- Parser for a single point. Unfortunately also needs to check for end of
-- input, for reasons I'm not 100% sure about.
pointP :: Parser Point
pointP = (one <* endOfInput) <|> (two <* endOfInput)
  where
    two = P2 <$> double
             <*  skipSpace
             <*> double
    one = P1 <$> double

-- | Parser for a single message, of the form "chartID: d1 d2"
message :: Parser Message
message = Message <$> stringLiteral
                  <*  char ':'
                  <*  skipSpace
                  <*> pointP

-- | Runs the message parser over a ByteString.
parseMessage :: ByteString -> Either String Message
parseMessage = parseOnly message

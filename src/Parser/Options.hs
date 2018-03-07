{-# LANGUAGE OverloadedStrings #-}

module Parser.Options
  ( Parser
  , parseChart
  ) where

import           Control.Applicative        (empty)
import           Control.Lens
import           Data.Char                  (GeneralCategory (..))
import           Data.Default               (def)
import           Data.Void                  (Void)

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Chart
import           Chart.Types                (Chart, PlotStyle (..), Subchart)


type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

stringLiteral :: Parser Text
stringLiteral = lexeme $
  T.pack <$> some (alphaNumChar <|> charCategory DashPunctuation)

parseChartStyle :: Parser PlotStyle
parseChartStyle = lexeme $
      (string "line"      >> return LinePlot)
  <|> (string "scatter"   >> return ScatterPlot)
  <|> (string "histogram" >> return Histogram)

parseSubchart :: Parser Subchart
parseSubchart = do
  subchartLabel <- stringLiteral
  subchartStyle <- parseChartStyle
  return $ Chart.label .~ subchartLabel
         $ Chart.style .~ subchartStyle
         $ def

parseChart :: Parser Chart
parseChart = do
  chartTitle <- stringLiteral
  subcharts  <- some parseSubchart
  return $ Chart.title     .~ chartTitle
         $ Chart.subcharts .~ subcharts
         $ def

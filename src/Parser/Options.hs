{-# LANGUAGE OverloadedStrings #-}

module Parser.Options
  ( parseChart
  ) where

import qualified Chart
import           Chart.Types          (Chart, ChartType (..), Subchart)
import           Control.Lens
import           Data.Default         (def)
import           Parser.Generic

import           Text.Megaparsec
import           Text.Megaparsec.Char

parseChartType :: Parser ChartType
parseChartType = lexeme $
      (string "line"    >> return Line)
  <|> (string "scatter" >> return Scatter)
  <|> (string "series"  >> return TimeSeries)

parseSubchart :: Parser Subchart
parseSubchart = do
  subchartLabel <- stringLiteral
  subchartType <- parseChartType
  return $ Chart.label   .~ subchartLabel
         $ Chart.dataset .~ Chart.newDataset subchartType
         $ def

parseChart :: Parser Chart
parseChart = do
  chartTitle <- stringLiteral
  subcharts  <- some parseSubchart
  return $ Chart.title     .~ chartTitle
         $ Chart.subcharts .~ subcharts
         $ def

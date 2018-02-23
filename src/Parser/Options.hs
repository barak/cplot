{-# LANGUAGE OverloadedStrings #-}

module Parser.Options
  ( parseChart
  ) where

import qualified Chart
import           Chart.Types          (Chart, PlotStyle (..), Subchart)
import           Control.Lens
import           Data.Default         (def)
import           Parser.Generic

import           Text.Megaparsec
import           Text.Megaparsec.Char

parseChartStyle :: Parser PlotStyle
parseChartStyle = lexeme $
      (string "line"    >> return LinePlot)
  <|> (string "scatter" >> return ScatterPlot)

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

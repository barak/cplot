module Parser.Options
  ( Parser
  , parseChart
  ) where

import           Control.Lens
import           Data.Char                  (GeneralCategory (..))
import           Data.Default               (def)

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Dataset                    (Dataset, Point)
import qualified Dataset.Backend.Histogram  as Histogram
import qualified Dataset.Backend.Line       as Line
import qualified Dataset.Backend.Scatter    as Scatter

import           Buffer                     (Buffer)
import qualified Buffer.Backend.MinMax      as MMB
import qualified Buffer.Backend.Queue       as QB

import qualified Chart.Types                as Chart
import           Chart.Types                (Chart, PlotStyle (..), Subchart)

import           Parser.Types


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
  return $ Chart.label   .~ subchartLabel
         $ Chart.style   .~ subchartStyle
         $ Chart.buffer  .~ mkBufferFor subchartStyle
         $ Chart.dataset .~ mkDatasetFor subchartStyle
         $ def

parseChart :: Parser Chart
parseChart = do
  chartTitle <- stringLiteral
  subcharts  <- some parseSubchart
  return $ Chart.title     .~ chartTitle
         $ Chart.subcharts .~ subcharts
         $ def

mkBufferFor :: PlotStyle -> Buffer Point
mkBufferFor = \case
  LinePlot    -> MMB.minMaxBuffer
  ScatterPlot -> QB.queueBuffer
  Histogram   -> QB.queueBuffer

mkDatasetFor :: PlotStyle -> Dataset Point
mkDatasetFor = \case
  LinePlot    -> Line.lineDataset
  ScatterPlot -> Scatter.scatterDataset
  Histogram   -> Histogram.histogramDataset

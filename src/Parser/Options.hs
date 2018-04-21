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


-- | Parser for a string literal. Takes alphanumeric characters and dash
--   punctuation.
stringLiteral :: Parser Text
stringLiteral = lexeme $
  T.pack <$> some (alphaNumChar <|> charCategory DashPunctuation)

-- | Parser for a plot style. Just checks if any one of "line", "scatter", or
--   "histogram" matches, otherwise it fails.
parseChartStyle :: Parser PlotStyle
parseChartStyle = lexeme $
      (string "line"      >> return LinePlot)
  <|> (string "scatter"   >> return ScatterPlot)
  <|> (string "histogram" >> return Histogram)

-- | Parser for a subchart.
parseSubchart :: Parser Subchart
parseSubchart = do
  subchartLabel <- stringLiteral
  subchartStyle <- parseChartStyle
  return $ Chart.label   .~ subchartLabel
         $ Chart.style   .~ subchartStyle
         $ Chart.buffer  .~ mkBufferFor subchartStyle
         $ Chart.dataset .~ mkDatasetFor subchartStyle
         $ def

-- | Parser for a chart.
parseChart :: Parser Chart
parseChart = do
  chartTitle <- stringLiteral
  subcharts  <- some parseSubchart
  return $ Chart.title     .~ chartTitle
         $ Chart.subcharts .~ subcharts
         $ def

-- | Provides a default buffer for each chart style. Can be changed at runtime.
mkBufferFor :: PlotStyle -> Buffer Point
mkBufferFor = \case
  LinePlot    -> MMB.minMaxBuffer
  ScatterPlot -> QB.queueBuffer
  Histogram   -> QB.queueBuffer

-- | Provides a default dataset for each chart style. Can, in principle, be
--   changed at runtime, but this functionality is not exposed yet.
mkDatasetFor :: PlotStyle -> Dataset Point
mkDatasetFor = \case
  LinePlot    -> Line.dataset
  ScatterPlot -> Scatter.dataset
  Histogram   -> Histogram.dataset

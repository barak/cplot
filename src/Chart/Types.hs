{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}

module Chart.Types where

import           Control.Lens
import           Data.Default
import           Data.Text                (Text)

import           Buffer                   (Buffer)
import qualified Buffer.Backend.MinMax    as MMB

import           Dataset                  (Dataset, Point(..))
import qualified Dataset.Backend.Line     as Line


-- | Internal chart data type. We compute using this type, then map it to a view
--   later on. This keeps the chart code separate from the rendering.
data Chart = Chart
  { _title       :: Text
  , _subcharts   :: [Subchart]
  , _axisScaling :: AxisScaling
  }

-- | Represents a single data/style group inside a chart.
data Subchart = Subchart
  { _label         :: Text
  , _buffer        :: Buffer Point
  , _dataset       :: Dataset Point
  , _style         :: PlotStyle
  , _numDataPoints :: Int
  , _maxDataPoints :: Int
  }

data AxisScaling
  = LinearScaling
  | LogScaling

data PlotStyle
  = LinePlot
  | ScatterPlot
  | Histogram

makeLenses ''Chart
makeLenses ''Subchart

-- Temporary function. Defines an ordering of points by their second component.
second :: Point -> Point -> Ordering
second (P2 _ y) (P2 _ y') = compare y y'
second p        p'        = compare p p'

--------------------------------------------------------------------------------
-- DEFAULT INSTANCES

instance Default Chart where
  def = Chart
    { _title       = "chart"
    , _subcharts   = []
    , _axisScaling = LinearScaling
    }

instance Default Subchart where
  def = Subchart
    { _label         = "label"
    , _buffer        = MMB.minMaxBufferBy second
    , _dataset       = Line.dataset
    , _style         = LinePlot
    , _numDataPoints = 0
    , _maxDataPoints = 500
    }

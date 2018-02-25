{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Chart.Types where

import           Control.Lens
import           Data.Default
import           Data.Text    (Text)
import           MinMaxQueue  (MinMaxQueue)

type Point   = (Double, Double)
type Dataset = MinMaxQueue Point

data Chart = Chart
  { _title     :: Text
  , _subcharts :: [Subchart]
  }

-- | Represents a single data/style group inside a chart
data Subchart = Subchart
  { _label         :: Text
  , _dataset       :: Dataset
  , _style         :: PlotStyle
  , _xAxisBounds   :: (Double, Double)
  , _numDataPoints :: Int
  -- consider making this 'Maybe Int', in case we want an unbounded dataset
  , _maxDataPoints :: Int
  }

data PlotStyle
  = LinePlot
  | ScatterPlot
  | Histogram   -- the plot thickens haHAA

makeLenses ''Chart
makeLenses ''Subchart

--------------------------------------------------------------------------------
-- DEFAULT INSTANCES

instance Default Chart where
  def = Chart
    { _title     = "chart"
    , _subcharts = []
    }

instance Default Subchart where
  def = Subchart
    { _label         = "label"
    , _dataset       = def
    , _style         = LinePlot
    , _xAxisBounds   = (0, 1)
    , _numDataPoints = 0
    , _maxDataPoints = 10000
    }

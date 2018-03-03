{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Chart.Types where

import           Control.Lens
import           Data.Default
import           Data.Text    (Text)
import           MinMaxBuffer (MinMaxBuffer)
import qualified MinMaxBuffer as MMB
import           MinMaxQueue  (MinMaxQueue)
import qualified MinMaxQueue  as MMQ


type Point   = (Double, Double)
type Dataset = MinMaxQueue Point

data Chart = Chart
  { _title     :: Text
  , _subcharts :: [Subchart]
  }

-- | Represents a single data/style group inside a chart
data Subchart = Subchart
  { _label         :: Text
  , _buffer        :: MinMaxBuffer Point
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
    , _buffer        = MMB.empty
    , _dataset       = MMQ.empty
    , _style         = LinePlot
    , _xAxisBounds   = (0, 1)
    , _numDataPoints = 0
    , _maxDataPoints = 5000
    }

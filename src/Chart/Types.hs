{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Chart.Types where

import           Control.Lens
import           Data.Default
import           Data.Ord              (comparing)
import           Data.Text             (Text)

import           Buffer                (Buffer)
import qualified Buffer.Backend.MinMax as MMB
import qualified Buffer.Backend.Queue  as QB

import           MinMaxQueue           (MinMaxQueue)
import qualified MinMaxQueue           as MMQ


type Point   = (Double, Double)
type Dataset = MinMaxQueue Point

data Chart = Chart
  { _title     :: Text
  , _subcharts :: [Subchart]
  }

-- | Represents a single data/style group inside a chart
data Subchart = Subchart
  { _label         :: Text
  , _buffer        :: Buffer (Double, Double)
  , _dataset       :: Dataset
  , _style         :: PlotStyle
  , _xAxisBounds   :: (Double, Double)
  , _numDataPoints :: Int
  , _maxDataPoints :: Int
  }

data PlotStyle
  = LinePlot
  | ScatterPlot
  | Histogram

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
    , _buffer        = MMB.minMaxBufferBy (comparing snd) -- QB.queueBuffer
    , _dataset       = MMQ.empty
    , _style         = LinePlot
    , _xAxisBounds   = (0, 1)
    , _numDataPoints = 0
    , _maxDataPoints = 500
    }

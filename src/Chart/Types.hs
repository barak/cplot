{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
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


data Chart = Chart
  { _title     :: Text
  , _subcharts :: [Subchart]
  }

-- | Represents a single data/style group inside a chart
data Subchart = Subchart
  { _label         :: Text
  , _buffer        :: Buffer Point
  , _dataset       :: Dataset Point
  , _style         :: PlotStyle
  , _numDataPoints :: Int
  , _maxDataPoints :: Int
  }

data PlotStyle
  = LinePlot
  | ScatterPlot
  | Histogram

makeLenses ''Chart
makeLenses ''Subchart

-- temporary function
second :: Point -> Point -> Ordering
second (Point _ y) (Point _ y') = compare y y'
second p           p'           = compare p p'

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
    , _buffer        = MMB.minMaxBufferBy second
    , _dataset       = Line.lineDataset
    , _style         = LinePlot
    , _numDataPoints = 0
    , _maxDataPoints = 500
    }

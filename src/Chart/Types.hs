{-# LANGUAGE TemplateHaskell #-}

module Chart.Types where

import           Control.Lens
import           Data.Default
import           Data.DList               (DList)
import qualified Data.DList               as DList
import qualified Graphics.Rendering.Chart as Chart


data Chart = Chart
  { _title     :: String
  , _subcharts :: [Subchart]
  , _rectSize  :: Chart.RectSize
  }

-- | Represents a single data/style group inside a chart
data Subchart = Subchart
  { _label   :: String
  , _dataset :: ChartData
  }

data ChartType
  = Line
  | Scatter
  | TimeSeries

data ChartData
  = LineData       (DList (Double, Double))
  | ScatterData    (DList (Double, Double))
  | TimeSeriesData (DList Double)

makeLenses ''Chart
makeLenses ''Subchart

--------------------------------------------------------------------------------
-- DEFAULT INSTANCES

instance Default Chart where
  def = Chart
    { _title     = "chart"
    , _subcharts = []
    , _rectSize  = (0, 0)
    }

instance Default Subchart where
  def = Subchart
    { _label   = "label"
    , _dataset = def
    }

instance Default ChartData where
  def = LineData DList.empty

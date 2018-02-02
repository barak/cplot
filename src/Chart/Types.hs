{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Chart.Types where

import           Control.Lens
import           Data.Default
import           Data.Sequence            (Seq, empty)
import           Data.Text                (Text)
import qualified Graphics.Rendering.Chart as Chart


data Chart = Chart
  { _title     :: Text
  , _subcharts :: [Subchart]
  , _rectSize  :: Chart.RectSize
  }

-- | Represents a single data/style group inside a chart
data Subchart = Subchart
  { _label         :: Text
  , _dataset       :: ChartData
  , _xAxisBounds   :: (Double, Double)
  , _numDataPoints :: Int
  , _maxDataPoints :: Int
  }

data ChartType
  = Line
  | Scatter

data ChartData
  = LineData       (Seq (Double, Double))
  | ScatterData    (Seq (Double, Double))

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
    { _label         = "label"
    , _dataset       = def
    , _xAxisBounds   = (0, 1)
    , _numDataPoints = 0
    , _maxDataPoints = 1000
    }

instance Default ChartData where
  def = LineData empty

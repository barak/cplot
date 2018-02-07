{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Chart.Types where

import           Control.Lens
import           Data.Default
import           Data.Text                (Text)
import           MinMaxQueue              (MinMaxQueue)
import qualified MinMaxQueue              as MMQ

data Chart = Chart
  { _title     :: Text
  , _subcharts :: [Subchart]
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
  = LineData    (MinMaxQueue (Double, Double))
  | ScatterData (MinMaxQueue (Double, Double))

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
    , _xAxisBounds   = (0, 1)
    , _numDataPoints = 0
    , _maxDataPoints = 10000
    }

instance Default ChartData where
  def = LineData MMQ.empty

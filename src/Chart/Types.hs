{-# LANGUAGE TemplateHaskell #-}

-- Should use the default class, messy to expose this much of each data type.
-- At least smart constructors would be more manageable
module Chart.Types
  ( Chart(Chart)
  , ChartType(..)
  , ChartData(..)

  -- Chart lenses
  , chartTitle
  , chartData
  , needsRedraw
  ) where

import           Control.Lens
import           Data.DList   (DList)
import           Data.IORef   (IORef)

data Chart = Chart
  { _chartTitle  :: String
  , _chartData   :: IORef [ChartData]
  , _needsRedraw :: IORef Bool
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

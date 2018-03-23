module Chart
  ( Chart
  , Subchart

  -- Chart lenses
  , title
  , subcharts

  -- Subchart lenses
  , label
  , buffer
  , dataset
  , style
  , numDataPoints
  , maxDataPoints

  -- stuff from this module
  , Chart.renderChart
  , pushToBuffer
  , flushBufferToDataset
  , setLinearAxis
  , setLogAxis
  , setConfig
  , setMaxDataPoints
  ) where

import           Control.Lens

import qualified Buffer
import qualified Dataset

import qualified Chart.Backend.Chart                    as Chart
import           Chart.Types

import           App.Types


pushToBuffer :: Dataset.Point -> Subchart -> Subchart
pushToBuffer p chart = chart & buffer %~ Buffer.put p

flushBufferToDataset :: Subchart -> Subchart
flushBufferToDataset subchart = foldr pushToDataset subchart' elems
  where
    (emptyBuf, elems) = Buffer.flush (subchart^.buffer)
    subchart'         = subchart & buffer .~ emptyBuf

pushToDataset :: Dataset.Point -> Subchart -> Subchart
pushToDataset p subchart =
  subchart & numDataPoints .~ min (numPoints + 1) maxPoints
           & dataset       %~ insert p
  where
    numPoints = subchart^.numDataPoints
    maxPoints = subchart^.maxDataPoints
    apply f n = foldr (.) id (replicate n f)
    insert point =
      Dataset.push point . apply Dataset.removeEnd (numPoints - maxPoints + 1)

setLinearAxis, setLogAxis :: Chart -> Chart
setLinearAxis chart = chart & axisScaling .~ LinearScaling
setLogAxis    chart = chart & axisScaling .~ LogScaling

setConfig :: AppConfig -> Subchart -> Subchart
setConfig conf subchart =
  case subchart^.style of
    LinePlot    -> subchart & maxDataPoints .~ conf^.lineConfig.cycleAfter
    _           -> subchart

setMaxDataPoints :: Int -> Subchart -> Subchart
setMaxDataPoints n subchart = subchart & maxDataPoints .~ n

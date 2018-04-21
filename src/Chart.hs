-- | Most things in this module are reasonably understandable (for varying
--   definitions of reasonable). Keep in mind that (using JavaScript dot
--   accessor notation):
--
--     1. a^.b.c.d    ==  a.b.c.d
--     2. a & b .~ c  ==  a.b = c;
--     3. a & b %~ f  ==  a.b = f(a.b)
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
  , setMMQ
  , setDefaultBuffer
  ) where

import           Control.Lens

import qualified Buffer
import qualified Dataset

import qualified Buffer.Backend.MinMax as MMQ
import qualified Buffer.Backend.Queue  as Queue

import qualified Chart.Backend.Chart   as Chart
import           Chart.Types

import           App.Types


-- | Push single point into the chart's buffer.
pushToBuffer :: Dataset.Point -> Subchart -> Subchart
pushToBuffer p chart = chart & buffer %~ Buffer.put p


-- | Drains a chart's buffer into its dataset.
flushBufferToDataset :: Subchart -> Subchart
flushBufferToDataset subchart = foldr pushToDataset subchart' elems
  where
    (emptyBuf, elems) = Buffer.flush (subchart^.buffer)
    subchart'         = subchart & buffer .~ emptyBuf


-- | Directly inserts a single point into a chart's dataset. Internal, should
--   not be exported as part of the chart's public API.
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


setMaxDataPoints :: Int -> Subchart -> Subchart
setMaxDataPoints n subchart = subchart & maxDataPoints .~ n


setMMQ :: Subchart -> Subchart
setMMQ subchart = subchart & buffer .~ MMQ.minMaxBuffer


setDefaultBuffer :: Subchart -> Subchart
setDefaultBuffer subchart = subchart & buffer .~ Queue.queueBuffer


-- | Update a chart based on configuration values. There's probably a better way
--   to get the behaviour I need from this function but it will do for now.
setConfig :: AppConfig -> Subchart -> Subchart
setConfig conf subchart =
  case subchart^.style of
    LinePlot    -> subchart & maxDataPoints .~ conf^.lineConfig.cycleAfter
    _           -> subchart

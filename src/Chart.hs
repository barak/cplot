{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}

module Chart
  ( Chart
  , Subchart

  -- Chart lenses
  , title
  , subcharts

  -- Subchart lenses
  , label
  , dataset
  , style
  , numDataPoints
  , maxDataPoints

  -- stuff from this module
  , renderChart
  , pushToBuffer
  , drainBufferToDataset
  ) where

import           Control.Lens                           hiding ((:<), (<|),
                                                         (|>))
import           Control.Monad                          (void)
import           Data.Default                           (def)
import           Data.Text                              (unpack)
import           Data.Maybe                             (catMaybes)

import qualified Buffer
import qualified Dataset

import qualified Graphics.Rendering.Cairo               as Cairo
import qualified Graphics.Rendering.Chart               as Chart
import           Graphics.Rendering.Chart.Backend.Cairo (defaultEnv, runBackend)

import           Chart.Types


-- | Helper for creating heterogeneous lists of plottable types.
data Plottable x y = forall a. Chart.ToPlot a => MkPlottable (a x y)

plottableToPlot :: Plottable x y -> Chart.Plot x y
plottableToPlot (MkPlottable p) = Chart.toPlot p

renderChart :: Chart -> Chart.RectSize -> Cairo.Render ()
renderChart chart rect =
  void $ runBackend (defaultEnv Chart.vectorAlignmentFns)
                    (Chart.render renderable rect)
  where
    renderable :: Chart.Renderable ()
    renderable = Chart.toRenderable layout

    layout
      = Chart.layout_title .~ unpack (chart^.title)
      $ Chart.layout_plots .~ map plottableToPlot plots
      $ Chart.layout_x_axis . Chart.laxis_generate .~ const Chart.AxisData
        { Chart._axis_visibility = def
        , Chart._axis_viewport   = Chart.vmap   (minX, maxX)
        , Chart._axis_tropweiv   = Chart.invmap (minX, maxX)
        , Chart._axis_ticks      = def
        -- FIXME: the two below need fixing (when I figure out how to fix them)
        , Chart._axis_grid       = def
        , Chart._axis_labels     = def
        }
      $ def

    plots = makePlottable <$> chart^.subcharts

    bounds = catMaybes
      [ Dataset.xbounds (subchart^.dataset) | subchart <- chart^.subcharts ]

    -- FIXME: BAD, fails if empty
    minX = minimum (map fst bounds)
    maxX = maximum (map snd bounds)

-- this function (and much of this module) needs to change to accomodate
-- different kinds of data for the charts
makePlottable :: Subchart -> Plottable Double Double
makePlottable subchart =
  case view style subchart of
    LinePlot -> MkPlottable
      $ Chart.plot_lines_title  .~ l
      $ Chart.plot_lines_values .~ [ map (\(Dataset.Point x y) -> (x, y)) (Dataset.toList d)]
      $ def

    ScatterPlot -> MkPlottable
      $ Chart.plot_points_title  .~ l
      $ Chart.plot_points_values .~ map (\(Dataset.Point x y) -> (x, y)) (Dataset.toList d)
      $ def

    Histogram -> undefined
  where
    d = subchart^.dataset
    l = unpack (subchart^.label)

pushToBuffer :: Dataset.Point -> Subchart -> Subchart
pushToBuffer p chart = chart & buffer %~ Buffer.put p

drainBufferToDataset :: Subchart -> Subchart
drainBufferToDataset subchart = foldr pushToDataset subchart' elems
  where
    (emptyBuf, elems) = Buffer.drain (subchart^.buffer)
    subchart'         = subchart & buffer .~ emptyBuf

pushToDataset :: Dataset.Point -> Subchart -> Subchart
pushToDataset p subchart =
  subchart & numDataPoints +~ (if nPoints >= maxPoints then 0 else 1)
           & dataset       %~ maybeInsert p
  where
    nPoints    = subchart^.numDataPoints
    maxPoints  = subchart^.maxDataPoints
    maybeInsert point d
      | nPoints >= maxPoints = Dataset.push point (Dataset.removeEnd d)
      | otherwise            = Dataset.push point d

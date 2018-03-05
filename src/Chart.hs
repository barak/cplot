{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}

module Chart
  ( Chart
  , Subchart
  , Point

  -- Chart lenses
  , title
  , subcharts

  -- Subchart lenses
  , label
  , dataset
  , style
  , xAxisBounds
  , numDataPoints
  , maxDataPoints

  -- stuff from this module
  , renderChart
  , pushToBuffer
  , drainBufferToDataset
  , pushPoint
  , pushPopPoint
  ) where

import           Control.Lens                           hiding ((:<), (<|),
                                                         (|>))
import           Control.Monad                          (void)
import           Data.Default                           (def)
import           Data.Ord                               (comparing)
import           Data.Text                              (unpack)

import qualified MinMaxBuffer                           as MMB
import qualified MinMaxQueue                            as MMQ

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
        , Chart._axis_viewport   = Chart.vmap   (minXChart, maxXChart)
        , Chart._axis_tropweiv   = Chart.invmap (minXChart, maxXChart)
        , Chart._axis_ticks      = def
        -- FIXME: the two below need fixing (when I figure out how to fix them)
        , Chart._axis_grid       = def
        , Chart._axis_labels     = def
        }
      $ def

    plots = makePlottable <$> chart^.subcharts

    -- these are necessary if you wish to have multiple plots on the same chart.
    minXChart = minimum [ minXVal (subchart^.dataset) | subchart <- chart^.subcharts ]
    maxXChart = maximum [ maxXVal (subchart^.dataset) | subchart <- chart^.subcharts ]

    minXVal d = let Just p = MMQ.minimum d in fst p
    maxXVal d = let Just p = MMQ.maximum d in fst p

makePlottable :: Subchart -> Plottable Double Double
makePlottable subchart =
  case view style subchart of
    LinePlot -> MkPlottable
      $ Chart.plot_lines_title  .~ l
      $ Chart.plot_lines_values .~ [MMQ.toList d]
      $ def

    ScatterPlot -> MkPlottable
      $ Chart.plot_points_title  .~ l
      $ Chart.plot_points_values .~ MMQ.toList d
      $ def
  where
    d = subchart^.dataset
    l = unpack (subchart^.label)

pushToBuffer :: Point -> Subchart -> Subchart
pushToBuffer p chart = chart & buffer %~ MMB.putBy (comparing snd) p

drainBufferToDataset :: Subchart -> Subchart
drainBufferToDataset subchart = foldr pushToDataset subchart' elems
  where
    elems     = MMB.drain (subchart^.buffer)
    subchart' = subchart & buffer .~ MMB.empty

pushToDataset :: Point -> Subchart -> Subchart
pushToDataset p subchart =
  subchart & numDataPoints +~ 1
           & dataset       %~ dataset'
  where
    nPoints    = subchart^.numDataPoints
    maxPoints  = subchart^.maxDataPoints
    dataset' d =
      if nPoints >= maxPoints
        then snd (pushPopPoint p d)
        else pushPoint p d

popPoint :: Dataset -> (Point, Dataset)
popPoint d = case MMQ.pop d of
  Just (p, d') -> (p, d')
  Nothing      -> ((0, 0), d)

pushPoint :: Point -> Dataset -> Dataset
pushPoint = MMQ.push

pushPopPoint :: Point -> Dataset -> (Point, Dataset)
pushPopPoint p d = (p', pushPoint p d')
  where (p', d') = popPoint d

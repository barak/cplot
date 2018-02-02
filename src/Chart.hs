{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}

module Chart
  ( Chart
  , ChartType
  , ChartData
  , Subchart

  -- Chart lenses
  , title
  , subcharts
  , rectSize

  -- Subchart lenses
  , label
  , dataset
  , xAxisBounds
  , numDataPoints
  , maxDataPoints

  -- stuff from this module
  , renderChart
  , pushPoint
  , newDataset
  ) where

import           Control.Lens                           hiding ((:<), (<|),
                                                         (|>))
import           Control.Monad                          (void)
import           Data.Default                           (def)
import           Data.Foldable                          (toList)
import           Data.Text                              (unpack)

import           Data.Sequence                          (Seq, ViewL (..),
                                                         ViewR (..), viewl,
                                                         viewr, (<|), (|>))
import qualified Data.Sequence                          as Seq

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
  void $ runBackend (defaultEnv Chart.bitmapAlignmentFns)
                    (Chart.render renderable rect)
  where
    renderable :: Chart.Renderable ()
    renderable = Chart.toRenderable layout

    layout
      = Chart.layout_title .~ unpack (chart ^. title)
      $ Chart.layout_plots .~ map plottableToPlot plots
      $ Chart.layout_x_axis . Chart.laxis_generate .~ const Chart.AxisData
        { Chart._axis_visibility = def
        , Chart._axis_viewport   = Chart.vmap (minXChart, maxXChart)
        , Chart._axis_tropweiv   = Chart.invmap (minXChart, maxXChart)
        , Chart._axis_ticks      = []
        -- FIXME: the two below need fixing (when I figure out how to fix them)
        , Chart._axis_grid       = def
        , Chart._axis_labels     = def
        }
      $ def

    plots = makePlottable <$> chart ^. subcharts

    -- wasteful to constantly recompute these, make the current max/min
    -- part of the chart dataset so they can be computed as points are added.
    minXChart = minimum [ minXVal (subchart ^. dataset) | subchart <- chart ^. subcharts ]
    maxXChart = maximum [ maxXVal (subchart ^. dataset) | subchart <- chart ^. subcharts ]

    minXVal = \case
      LineData d    -> minimum (fmap fst d)
      ScatterData d -> minimum (fmap fst d)

    maxXVal = \case
      LineData d    -> maximum (fmap fst d)
      ScatterData d -> maximum (fmap fst d)

makePlottable :: Subchart -> Plottable Double Double
makePlottable subchart =
  case view dataset subchart of
    LineData d -> MkPlottable
      $ Chart.plot_lines_title  .~ unpack (subchart ^. label)
      $ Chart.plot_lines_values .~ [toList d]
      $ def

    ScatterData d -> MkPlottable
      $ Chart.plot_points_title  .~ unpack (subchart ^. label)
      $ Chart.plot_points_values .~ toList d
      $ def

addPoint :: (Double, Double) -> ChartData -> ChartData
addPoint p = \case
  LineData d    -> LineData    (d |> p)
  ScatterData d -> ScatterData (d |> p)

pushPopPoint :: (Double, Double) -> ChartData -> ChartData
pushPopPoint p = \case
  LineData    d -> case viewl d of
    _ :< d' -> LineData (d' |> p)
    EmptyL  -> LineData (Seq.singleton p)

  ScatterData d -> case viewl d of
    _ :< d' -> ScatterData (d' |> p)
    EmptyL  -> ScatterData (Seq.singleton p)

pushPoint :: (Double, Double) -> Subchart -> Subchart
pushPoint p subchart
  | nPoints < maxPoints = subchart & dataset %~ addPoint p
  | otherwise           = subchart & dataset %~ pushPopPoint p
  where
    nPoints   = subchart ^. numDataPoints
    maxPoints = subchart ^. maxDataPoints

newDataset :: ChartType -> ChartData
newDataset = \case
  Line    -> LineData def
  Scatter -> ScatterData def

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

  -- Subchart lenses
  , label
  , dataset
  , xAxisBounds
  , numDataPoints
  , maxDataPoints

  -- stuff from this module
  , renderChart
  , pushPoint
  , pushPopPoint
  , newDataset
  ) where

import           Control.Lens                           hiding ((:<), (<|),
                                                         (|>))
import           Control.Monad                          (void)
import           Data.Default                           (def)
import           Data.Maybe                             (fromJust)
import           Data.Text                              (unpack)

-- import           Data.Sequence                          (ViewL (..), viewl,
--                                                          (|>))
-- import qualified Data.Sequence                          as Seq

import           MinMaxQueue                            (MinMaxQueue)
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
        , Chart._axis_viewport   = Chart.vmap   (minXChart, maxXChart)
        , Chart._axis_tropweiv   = Chart.invmap (minXChart, maxXChart)
        , Chart._axis_ticks      = def
        -- FIXME: the two below need fixing (when I figure out how to fix them)
        , Chart._axis_grid       = def
        , Chart._axis_labels     = def
        }
      $ def

    plots = makePlottable <$> chart ^. subcharts

    minXChart = minimum [ minXVal (subchart ^. dataset) | subchart <- chart ^. subcharts ]
    maxXChart = maximum [ maxXVal (subchart ^. dataset) | subchart <- chart ^. subcharts ]

    minXVal = \case
      LineData d    -> fst $ fromJust (MMQ.minimum d)
      ScatterData d -> fst $ fromJust (MMQ.minimum d)

    maxXVal = \case
      LineData d    -> fst $ fromJust (MMQ.maximum d)
      ScatterData d -> fst $ fromJust (MMQ.maximum d)

makePlottable :: Subchart -> Plottable Double Double
makePlottable subchart =
  case view dataset subchart of
    LineData d -> MkPlottable
      $ Chart.plot_lines_title  .~ unpack (subchart ^. label)
      $ Chart.plot_lines_values .~ [MMQ.toList d]
      $ def

    ScatterData d -> MkPlottable
      $ Chart.plot_points_title  .~ unpack (subchart ^. label)
      $ Chart.plot_points_values .~ MMQ.toList d
      $ def

popPoint :: ChartData -> ((Double, Double), ChartData)
popPoint = \case
  LineData d -> case MMQ.pop d of
    Just (p, d') -> (p, LineData d')
    Nothing      -> ((0, 0), LineData d)

  ScatterData d -> case MMQ.pop d of
    Just (p, d') -> (p, ScatterData d')
    Nothing      -> ((0, 0), ScatterData d)

pushPoint :: (Double, Double) -> ChartData -> ChartData
pushPoint p = \case
  LineData    d -> LineData    (MMQ.push p d)
  ScatterData d -> ScatterData (MMQ.push p d)

pushPopPoint :: (Double, Double) -> ChartData -> ((Double, Double), ChartData)
pushPopPoint p d = (p', pushPoint p d')
  where (p', d') = popPoint d

newDataset :: ChartType -> ChartData
newDataset = \case
  Line    -> LineData MMQ.empty
  Scatter -> ScatterData MMQ.empty

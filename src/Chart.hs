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

  -- stuff from this module
  , renderChart
  , addPoint
  ) where

import           Control.Lens
import           Control.Monad                          (void)
import           Data.Default                           (def)
import qualified Data.DList                             as DList

import qualified Graphics.Rendering.Chart               as Chart

import qualified Graphics.Rendering.Cairo               as Cairo
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
      = Chart.layout_title .~ chart ^. title
      $ Chart.layout_plots .~ map plottableToPlot plots
      $ def

    plots = makePlottable <$> chart ^. subcharts

    makePlottable :: Subchart -> Plottable Double Double
    makePlottable subchart =
      case view dataset subchart of
        LineData d -> MkPlottable
          $ Chart.plot_lines_title  .~ subchart ^. label
          $ Chart.plot_lines_values .~ [DList.toList d]
          $ def

        ScatterData d -> MkPlottable
          $ Chart.plot_points_title  .~ subchart ^. label
          $ Chart.plot_points_values .~ DList.toList d
          $ def

        TimeSeriesData _ -> error "not yet implemented"

addPoint :: (Double, Double) -> ChartData -> ChartData
addPoint p@(_, y) = \case
  LineData       d -> LineData       $ DList.snoc d p
  ScatterData    d -> ScatterData    $ DList.snoc d p
  TimeSeriesData d -> TimeSeriesData $ DList.snoc d y

module Chart.Backend.Chart
  ( renderChart
  ) where

import           Control.Lens
import           Control.Monad                          (void)
import           Data.Default                           (def)
import           Data.List.NonEmpty                     (nonEmpty)
import           Data.Maybe                             (catMaybes)
import           Data.Text                              (unpack)

import qualified Graphics.Rendering.Cairo               as Cairo
import qualified Graphics.Rendering.Chart               as Chart
import           Graphics.Rendering.Chart.Backend.Cairo (defaultEnv, runBackend)

import qualified Dataset

import           Chart.Types

-- | This type allows you to package all 'Renderable' (in the Chart backend sense)
--   values as a single type by hiding all information about that type, besides
--   the fact that it can be rendered.
data Renderable = forall r. Chart.ToRenderable r => MkRenderable r

-- | Given a cplot home-rolled 'Chart', produce a cairo rendered object, which
--   we can then paint to any canvas that supports such objects.
renderChart :: Chart -> Chart.RectSize -> Cairo.Render ()
renderChart chart rect =
  void $ runBackend (defaultEnv Chart.vectorAlignmentFns)
                    (Chart.render renderable rect)
  where
    -- Unwrap the Renderable wrapper and actually make the object renderable.
    render :: Renderable -> Chart.Renderable ()
    render (MkRenderable r) = Chart.toRenderable r

    renderable :: Chart.Renderable ()
    renderable = render layout

    layout =
      case chart^.axisScaling of
        LinearScaling -> MkRenderable
          $ Chart.layout_title  .~ unpack (chart^.title)
          $ Chart.layout_plots  .~ map makeLinearPlottable (chart^.subcharts)
          $ Chart.layout_x_axis .~ customLinAxis minX maxX
          $ Chart.layout_y_axis .~ customLinAxis minY maxY
          $ def

        LogScaling -> MkRenderable
          $ Chart.layout_title .~ unpack (chart^.title)
          $ Chart.layout_plots .~ map makeLogPlottable (chart^.subcharts)
          $ Chart.layout_x_axis .~ customLinAxis minX maxX
          $ Chart.layout_y_axis .~ customLogAxis minY maxY
          $ def

    -- The rest of the functions in here are mostly for computing axis bounds.
    -- It is annoyingly convoluted.

    customLinAxis (Just mn) (Just mx) | mn /= mx =
      Chart.laxis_override .~
        (\axisData -> Chart.axis_viewport .~ Chart.vmap   (mn, mx)
                    $ Chart.axis_tropweiv .~ Chart.invmap (mn, mx)
                    $ axisData) $ def
    customLinAxis _ _ = def

    customLogAxis (Just mn) (Just mx) | mn /= mx =
      Chart.laxis_override .~
        (\axisData -> Chart.axis_viewport .~ Chart.vmap   (Chart.LogValue mn, Chart.LogValue mx)
                    $ Chart.axis_tropweiv .~ Chart.invmap (Chart.LogValue mn, Chart.LogValue mx)
                    $ axisData) $ def
    customLogAxis _ _ = def

    xbounds = catMaybes
      [ Dataset.xbounds (subchart^.dataset) | subchart <- chart^.subcharts ]
    ybounds = catMaybes
      [ Dataset.ybounds (subchart^.dataset) | subchart <- chart^.subcharts ]

    (minX, maxX) = mins xbounds
    (minY, maxY) = mins ybounds

    mins bounds = (safeMinimum (map fst bounds), safeMaximum (map snd bounds))

    safeMinimum = fmap minimum . nonEmpty
    safeMaximum = fmap maximum . nonEmpty

-- | For linear axes, convert a 'Subchart' to a 'Plot'.
makeLinearPlottable :: Subchart -> Chart.Plot Double Double
makeLinearPlottable subchart =
  case subchart^.style of
    LinePlot -> Chart.toPlot
      $ Chart.plot_lines_title  .~ l
      $ Chart.plot_lines_values .~ [ unwrapPoints (Dataset.toList d) ]
      $ def

    ScatterPlot -> Chart.toPlot
      $ Chart.plot_points_title  .~ l
      $ Chart.plot_points_values .~ unwrapPoints (Dataset.toList d)
      $ def

    Histogram -> Chart.histToPlot
      $ Chart.plot_hist_title    .~ l
      $ Chart.plot_hist_bins     .~ 16
      $ Chart.plot_hist_values   .~ unwrapSingles (Dataset.toList d)
      $ Chart.plot_hist_no_zeros .~ False
      $ Chart.defaultNormedPlotHist

  where
    d = subchart^.dataset
    l = unpack (subchart^.label)

    unwrapSingles xs = [ x      | Dataset.P1 x  <- xs ]
    unwrapPoints  ps = [ (x, y) | Dataset.P2 x y <- ps ]

-- | For a logarithmic axis, convert a 'Subchart' to a 'Plot'.
makeLogPlottable :: Subchart -> Chart.Plot Double Chart.LogValue
makeLogPlottable subchart =
  case subchart^.style of
    LinePlot -> Chart.toPlot
      $ Chart.plot_lines_title  .~ l
      $ Chart.plot_lines_values .~ [ unwrapPoints (Dataset.toList d) ]
      $ def

    ScatterPlot -> Chart.toPlot
      $ Chart.plot_points_title  .~ l
      $ Chart.plot_points_values .~ unwrapPoints (Dataset.toList d)
      $ def

    Histogram -> Chart.histToPlot
      $ Chart.plot_hist_title     .~ l
      $ Chart.plot_hist_bins      .~ 16
      $ Chart.plot_hist_values    .~ unwrapSingles (Dataset.toList d)
      $ Chart.plot_hist_no_zeros  .~ False
      $ Chart.plot_hist_norm_func .~ norm
      $ Chart.defaultNormedPlotHist

  where
    d = subchart^.dataset
    l = unpack (subchart^.label)

    norm n y = Chart.LogValue (realToFrac y) / realToFrac n

    -- Yes, you read that correctly, whether the axis is linear or log, which
    -- really should be a property of the *axes*, is in fact a property of *each
    -- individual data point*. I cannot understand why this is the case.
    unwrapSingles xs = [ x | Dataset.P1 x  <- xs ]
    unwrapPoints  ps = [ (x, Chart.LogValue y) | Dataset.P2 x y <- ps ]

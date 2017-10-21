module Chart
  ( renderChart
  ) where

import           Control.Lens
import           Control.Monad                          (void)

import qualified Graphics.Rendering.Chart.Easy          as Chart

import qualified Graphics.Rendering.Cairo               as Cairo
import           Graphics.Rendering.Chart.Backend.Cairo (defaultEnv, runBackend)

-- | This function should take:
--      - AxisScaling [Linear|Log|...]
--      - Chart.RectSize (size of the chart)
--      - dataset ioref? not sure where exactly the best place to put this is
renderChart :: [[(Double, Double)]] -> Cairo.Render ()
renderChart dat = void $ runBackend (defaultEnv Chart.bitmapAlignmentFns)
                                    (Chart.render chart (800, 600))
  where
    chart :: Chart.Renderable ()
    chart = Chart.toRenderable $ do
      Chart.layout_title .= "sample chart"
      Chart.plot (Chart.line "sample" dat)

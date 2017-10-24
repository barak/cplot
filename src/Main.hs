{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.DList               as DList

import qualified Control.Concurrent       as CC
import qualified Data.IORef               as IORef

import           Control.Monad            (forever, void)
import           Control.Monad.Trans      (liftIO)

import           Pipes
import qualified Pipes.ByteString         as P

import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk          (AttrOp ((:=)))
import qualified Graphics.UI.Gtk          as Gtk

import           Control.FRPNow
import           Control.FRPNow.GTK

import qualified Chart
import qualified Parser


main :: IO ()
main = runNowGTK $ do

  mainWindow <- sync Gtk.windowNew
  sync $ Gtk.set mainWindow
    [ Gtk.windowTitle          := ("cplot" :: String)
    , Gtk.windowDefaultWidth   := 800
    , Gtk.windowDefaultHeight  := 600
    , Gtk.containerBorderWidth := 10
    ]

  sync $ bindQuit mainWindow

  chartCanvas <- sync Gtk.drawingAreaNew
  sync $ Gtk.set mainWindow
    [ Gtk.containerChild := chartCanvas ]

  datRef <- sync $ IORef.newIORef [DList.empty]

  let
    requestRedrawCanvas :: IO ()
    requestRedrawCanvas = Gtk.postGUIAsync $ Gtk.widgetQueueDraw chartCanvas

    parsePoint :: MonadIO m => Pipe P.ByteString (Maybe (Double, Double)) m ()
    parsePoint =
      forever $ do
        raw <- await
        case Parser.point raw of
          Left e  -> liftIO (putStr $ Parser.parseErrorPretty e) >> yield Nothing
          Right p -> yield (Just p)

    updateRefs :: MonadIO m => Consumer (Maybe (Double, Double)) m ()
    updateRefs =
      forever $ do
        point <- await
        case point of
          Nothing -> return ()
          Just pt -> liftIO $ IORef.modifyIORef datRef (\(x:xs) -> DList.snoc x pt : xs)

    pipe :: MonadIO m => Effect m ()
    pipe = P.stdin >-> parsePoint >-> updateRefs

    updateCanvas :: Cairo.Render ()
    updateCanvas = liftIO $ do
      dat <- IORef.readIORef datRef
      let renderable = Chart.renderChart (map DList.toList dat)

      Just win <- Gtk.widgetGetWindow chartCanvas
      Gtk.renderWithDrawWindow win renderable

  asyncOS $ forever $ do
    requestRedrawCanvas
    CC.threadDelay (1000000 `div` 30)

  -- attach updateCanvas to draw handler
  sync $ chartCanvas `Gtk.on` Gtk.draw $ updateCanvas

  asyncOS $ runEffect pipe
  sync $ Gtk.widgetShowAll mainWindow

bindQuit :: Gtk.WidgetClass a => a -> IO ()
bindQuit window = void $ window `Gtk.on` Gtk.deleteEvent $ liftIO Gtk.mainQuit >> return False

module Events
  ( handleEvent
  ) where

import qualified Data.Set                         as Set
import           Graphics.Gloss.Interface.IO.Game
import           Model

-- | Consider using lenses here if digging into the model becomes cumbersome
handleEvent :: Event -> Model -> IO Model
handleEvent event model

  -- | EventKey: Tracks key/button presses
  | EventKey (MouseButton LeftButton) Down _ (x, y) <- event
  = return $ model
      { buttonsPressed = Set.insert (MouseButton LeftButton)
                       $ buttonsPressed model
      , clickLoc       = (x, y)
      }

  | EventKey (MouseButton LeftButton) Up _ _ <- event
  = return $ model
      { buttonsPressed = Set.delete (MouseButton LeftButton)
                       $ buttonsPressed model
      }

  -- | EventMotion: Tracks position of the mouse (irrespective of buttons
  --   pressed)
  | EventMotion (x, y) <- event
  -- , MouseButton LeftButton `elem` buttonsPressed model
  = return $ model { cursorLoc = (realToFrac x, realToFrac y) }

  -- | EventResize: Tracks any resizing events and returns the new window size.
  | EventResize (x, y) <- event
  = return $ model { windowSize = (x, y) }

  | otherwise = return model

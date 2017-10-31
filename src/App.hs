module App
  ( App
  , AppEnv
  , AppConfig
  , AppState
  , HasAppOptions
  , HasAppConfig
  , HasAppState

  , runApp
  , newAppEnv

  -- AppEnv lenses
  , options
  , config
  , state

  -- AppState lenses
  , chartRefs
  ) where

import           Control.Monad        (void)
import           Control.Monad.Reader (runReaderT)

import           App.Types
import           Options


newAppEnv :: AppOptions -> AppConfig -> AppState -> AppEnv
newAppEnv = AppEnv

runApp :: App a -> AppEnv -> IO ()
runApp app = void . runReaderT (unApp app)

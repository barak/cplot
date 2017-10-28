module App
  ( App
  , AppEnv
  , AppConfig
  , AppState
  , HasAppOptions
  , HasAppConfig
  , HasAppState
  , HasAppLogger

  , runApp
  , newAppEnv

  -- AppEnv lenses
  , appOptions
  , appConfig
  , appState
  , appLogger

  -- AppState lenses
  , chartRefs
  ) where

import           Control.Monad        (void)
import           Control.Monad.Reader (runReaderT)

import           App.Types
import           Options


newAppEnv :: AppOptions -> AppConfig -> AppState -> (String -> IO ()) -> AppEnv
newAppEnv = AppEnv

runApp :: App a -> AppEnv -> IO ()
runApp app = void . runReaderT (unApp app)

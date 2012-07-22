{-# Language GeneralizedNewtypeDeriving #-}

module Graphics.UI.Oak.SDL where

import Graphics.UI.Oak.Basics

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import Control.Monad.State

data SurfaceData = SurfaceData {
    font :: TTF.Font
  , size :: Size
  } deriving (Eq, Show)


newtype Surface a = Surface (StateT SurfaceData IO a)
                    deriving (Monad, MonadIO)

runSurface :: Surface a -> SurfaceData -> IO (a, SurfaceData)
runSurface (Surface s) = runStateT s


getTextSize :: String -> Surface Size
getTextSize s = Surface $ do
  fnt <- gets font
  (w, h) <- liftIO $ TTF.textSize fnt s
  return $ Size w h


getSurfSize :: Surface Size
getSurfSize = Surface $ return =<< gets size


instance MonadSurface Surface where
  textSize = getTextSize
  surfSize = getSurfSize


newtype Frontend u a = Frontend (StateT u IO a)
                  deriving (Monad, MonadIO)


initSDL :: Frontend u ()
initSDL =
  liftIO $ do SDL.init [SDL.InitEverything]
              TTF.init
              SDL.setVideoMode 640 480 32 []
              SDL.setCaption "Jewelry" "Jewerly"
              return ()
  


instance MonadFrontend (Frontend u) where
  initialize = initSDL


runSDLFrontend :: Frontend u a -> u -> IO (a, u)
runSDLFrontend (Frontend stt) u = runStateT stt u

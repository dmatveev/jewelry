{-# Language GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Graphics.UI.Oak.SDL where

import Graphics.UI.Oak.Basics

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import Control.Monad.State
import Data.Mutators
import Data.Maybe (fromJust, mapMaybe)

data SurfaceData = SurfaceData {
    fontDesc  :: Font
  , size      :: Size
  , title     :: String
  , font      :: Maybe TTF.Font
  } deriving (Eq, Show)

genMutators ''SurfaceData


createSurface :: String -> Font -> Size -> SurfaceData
createSurface sTitle sFont sSize =
  SurfaceData { title     = sTitle
              , fontDesc  = sFont
              , size      = sSize
              , font      = Nothing
              }
  

newtype Frontend a = Frontend (StateT SurfaceData IO a)
                       deriving ( Monad
                                , MonadIO
                                , MonadState SurfaceData
                                )


initSDL :: Frontend ()
initSDL = do
  (Size w h) <- gets size
  (Font fntName fntSz) <- gets fontDesc
  strTitle <- gets title
  
  liftIO $ do SDL.init [SDL.InitEverything]
              TTF.init
              SDL.setVideoMode w h 32 []
              SDL.setCaption strTitle ""
              
  font <- liftIO $ TTF.openFont fntName fntSz
  modify $ \s -> setFont s (Just font)


collectSDLEvents :: IO [SDL.Event]
collectSDLEvents = reverse `liftM` (collect' [])
  where collect' es = do
          event <- SDL.pollEvent
          if event == SDL.NoEvent
          then return es
          else collect' $ event : es


toEvent :: SDL.Event -> Maybe Event
toEvent e = case e of
  SDL.Quit  -> Just Quit
  otherwise -> Nothing


getSDLEvents :: Frontend [Event]
getSDLEvents = do
  evs <- liftIO collectSDLEvents
  return $ mapMaybe toEvent evs


center :: Rect -> Size -> Rect
center (Rect x y (Size w h)) sz@(Size a b) = Rect xc yc sz
  where xc = x + (w - a) `div` 2
        yc = y + (h - b) `div` 2
  
renderString :: String -> Rect -> Frontend ()
renderString s (Rect x y (Size w h)) = do
  surf <- liftIO $ SDL.getVideoSurface
  fnt  <- liftM fromJust $ gets font
  text <- liftIO $ TTF.renderTextBlended fnt s (SDL.Color 255 255 255)
  liftIO $ SDL.blitSurface text Nothing surf (Just $ SDL.Rect x y w h)
  return ()


renderSDL :: Widget idt -> Rect -> Frontend ()

renderSDL (VBox items) _ =
  forM_ items $ \(LayoutItem _ w r) -> render w r

renderSDL (HBox items) _ =
  forM_ items $ \(LayoutItem _ w r) -> render w r

renderSDL (Stretch) _ = return ()

renderSDL (Label s)  rc = renderString s rc

renderSDL (Button s) rc = do
  sz <- textSize s
  renderString s (rc `center` sz)
  
endIterSDL :: Frontend ()
endIterSDL = liftIO $ do
  surf <- SDL.getVideoSurface
  SDL.flip surf
  SDL.delay 30
  SDL.fillRect surf Nothing (SDL.Pixel 0)
  return ()

instance MonadFrontend Frontend where
  initialize = initSDL
  getEvents = getSDLEvents
  render = renderSDL
  endIter = endIterSDL

getSDLTextSize :: String -> Frontend Size
getSDLTextSize s = do
  mfnt <- gets font
  (w, h) <- liftIO $ TTF.textSize (fromJust mfnt) s
  return $ Size w h


getSDLSurfSize :: Frontend Size
getSDLSurfSize = return =<< gets size


instance MonadSurface Frontend where
  textSize = getSDLTextSize
  surfSize = getSDLSurfSize


runSDLFrontend :: Frontend a -> SurfaceData -> IO a
runSDLFrontend (Frontend stt) sf = evalStateT stt sf

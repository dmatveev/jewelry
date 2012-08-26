{-# Language GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Graphics.UI.Oak.SDL where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

import Control.Monad (forM_)
import Control.Monad.State
import Data.Mutators
import Data.Maybe (fromJust, mapMaybe)
import Data.List (foldl')

import Graphics.UI.Oak.Basics
import Graphics.UI.Oak.Classes
import Graphics.UI.Oak.Widgets


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


keySymToEvt :: SDL.Keysym -> Maybe Key
keySymToEvt (SDL.Keysym k _ _) = lookup k table
  where table = [ (SDL.SDLK_LEFT,   ArrowLeft)
                , (SDL.SDLK_RIGHT,  ArrowRight)
                , (SDL.SDLK_DOWN,   ArrowDown)
                , (SDL.SDLK_UP,     ArrowUp)
                , (SDL.SDLK_RETURN, Return)
                ]


toEvent :: SDL.Event -> Maybe Event
toEvent e = case e of
  SDL.Quit        -> Just Quit
  (SDL.KeyDown k) -> liftM KeyDown $ keySymToEvt k
  otherwise       -> Nothing


getSDLEvents :: Frontend [Event]
getSDLEvents = do
  evs <- liftIO collectSDLEvents
  return $ mapMaybe toEvent evs


centered :: Rect -> Size -> Rect
centered (Rect x y (Size w h)) sz@(Size a b) = Rect xc yc sz
  where xc = x + (w - a) `div` 2
        yc = y + (h - b) `div` 2

        
toRect :: Rect -> SDL.Rect
toRect (Rect x y (Size w h)) = SDL.Rect x y w h


renderLine :: String -> Rect -> Frontend ()
renderLine line rc = do
    surf <- liftIO $ SDL.getVideoSurface
    fnt  <- liftM fromJust $ gets font
    text <- liftIO $ TTF.renderTextBlended fnt line cl
    liftIO $ SDL.blitSurface text Nothing surf (Just $ toRect rc)
    return ()
  where cl = SDL.Color 255 255 255


renderString :: String -> Rect -> Frontend ()
renderString str (Rect x y _) = do
  let ls = lines str
  sizes <- mapM getSDLLineSize ls
  let ys = scanl (+) y $ map snd sizes
  forM_ (zip3 ls ys sizes) $ \(l, y', (w, h)) ->
    renderLine l (Rect x y' (Size w h))
  
    
renderRect :: Rect -> (Int, Int, Int) -> Frontend ()
renderRect rc (r, g, b) = do
  surf <- liftIO $ SDL.getVideoSurface
  let pixf = SDL.surfaceGetPixelFormat surf
  liftIO $ do cl <- SDL.mapRGB pixf
                    (fromIntegral r)
                    (fromIntegral g)
                    (fromIntegral b)
              SDL.fillRect surf (Just $ toRect rc) cl
  return ()

blue = (0, 128, 255)

renderButton :: String -> WidgetState -> Rect -> Frontend ()
renderButton s st rc = do
    when (st == Focused) $ renderRect rc blue
    sz <- textSize s
    renderString s (rc `centered` sz)


renderSDL :: Widget idt -> WidgetState -> Rect -> Frontend ()
renderSDL w st rc = case w of
  (Label s)    -> renderString s rc
  (Button s)   -> renderButton s st rc
  (Line i)     -> renderRect rc blue
  otherwise  -> return ()
  
  
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


getSDLLineSize :: String -> Frontend (Int, Int)
getSDLLineSize line = do
  mfnt <- gets font
  liftIO $ TTF.textSize (fromJust mfnt) line
  
getSDLTextSize :: String -> Frontend Size
getSDLTextSize str = do
  mfnt <- gets font
  sizes <- mapM getSDLLineSize $ lines str
  let width  = foldl' max 0 $ map fst sizes
      height = foldl' (+) 0 $ map snd sizes
  return $ Size width height


getSDLSurfSize :: Frontend Size
getSDLSurfSize = return =<< gets size


instance MonadSurface Frontend where
  textSize = getSDLTextSize
  surfSize = getSDLSurfSize


runSDLFrontend :: Frontend a -> SurfaceData -> IO a
runSDLFrontend (Frontend stt) sf = evalStateT stt sf

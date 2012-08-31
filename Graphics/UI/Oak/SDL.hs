{-# LANGUAGE GeneralizedNewtypeDeriving,
             TemplateHaskell,
             FlexibleInstances,
             MultiParamTypeClasses #-}

module Graphics.UI.Oak.SDL where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

import Data.Char (toUpper)
import Data.List (foldl')
import Data.Mutators
import Data.Maybe (fromJust, mapMaybe)

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.State.Strict


import Graphics.UI.Oak.Basics
import Graphics.UI.Oak.Classes
import Graphics.UI.Oak.Widgets


data FrontendConfig u = FrontendConfig {
    fontDesc  :: Font
  , size      :: Size
  , title     :: String
  , font      :: Maybe TTF.Font
  , userData  :: u
  } deriving (Eq, Show)

genMutators ''Rect
genMutators ''FrontendConfig


fConfig :: String -> Font -> Size -> u -> FrontendConfig u
fConfig sTitle sFont sSize uData =
  FrontendConfig { title     = sTitle
                 , fontDesc  = sFont
                 , size      = sSize
                 , font      = Nothing
                 , userData  = uData
                 }
  

newtype Frontend u a = Frontend (StateT (FrontendConfig u) IO a)
                       deriving ( Monad
                                , MonadIO
                                , MonadState (FrontendConfig u)
                                )


initSDL :: Frontend u ()
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
keySymToEvt (SDL.Keysym k ms _) = pp <$> lookup k table
  where table = [ (SDL.SDLK_LEFT,      ArrowLeft)
                , (SDL.SDLK_RIGHT,     ArrowRight)
                , (SDL.SDLK_DOWN,      ArrowDown)
                , (SDL.SDLK_UP,        ArrowUp)
                , (SDL.SDLK_RETURN,    Return)
                , (SDL.SDLK_SPACE,     SpaceKey)
                , (SDL.SDLK_F10,       F10)
                , (SDL.SDLK_a,         Character 'a')
                , (SDL.SDLK_b,         Character 'b')
                , (SDL.SDLK_c,         Character 'c')
                , (SDL.SDLK_d,         Character 'd')
                , (SDL.SDLK_e,         Character 'e')
                , (SDL.SDLK_f,         Character 'f')
                , (SDL.SDLK_g,         Character 'g')
                , (SDL.SDLK_h,         Character 'h')
                , (SDL.SDLK_i,         Character 'i')
                , (SDL.SDLK_j,         Character 'j')
                , (SDL.SDLK_k,         Character 'k')
                , (SDL.SDLK_l,         Character 'l')
                , (SDL.SDLK_m,         Character 'm')
                , (SDL.SDLK_n,         Character 'n')
                , (SDL.SDLK_o,         Character 'o')
                , (SDL.SDLK_p,         Character 'p')
                , (SDL.SDLK_q,         Character 'q')
                , (SDL.SDLK_r,         Character 'r')
                , (SDL.SDLK_s,         Character 's')
                , (SDL.SDLK_t,         Character 't')
                , (SDL.SDLK_u,         Character 'u')
                , (SDL.SDLK_v,         Character 'v')
                , (SDL.SDLK_w,         Character 'w')
                , (SDL.SDLK_x,         Character 'x')
                , (SDL.SDLK_y,         Character 'y')
                , (SDL.SDLK_z,         Character 'z')
                , (SDL.SDLK_TAB,       Tab)
                , (SDL.SDLK_HOME,      Home)
                , (SDL.SDLK_END,       End)
                , (SDL.SDLK_BACKSPACE, Backspace)
                , (SDL.SDLK_DELETE,    Delete)
                ]
        pp evt = case evt of
          ch@(Character c) -> if or $ map (flip elem ms) shiftKeys
                              then Character $ toUpper c
                              else ch
          otherwise -> evt

        shiftKeys = [ SDL.KeyModLeftShift
                    , SDL.KeyModRightShift
                    , SDL.KeyModCaps
                    ]

toEvent :: SDL.Event -> Maybe Event
toEvent e = case e of
  SDL.Quit        -> Just Quit
  (SDL.KeyDown k) -> liftM KeyDown $ keySymToEvt k
  otherwise       -> Nothing


getSDLEvents :: Frontend u [Event]
getSDLEvents = do
  evs <- liftIO collectSDLEvents
  return $ mapMaybe toEvent evs


centered :: Rect -> Size -> Rect
centered (Rect x y (Size w h)) sz@(Size a b) = Rect xc yc sz
  where xc = x + (w - a) `div` 2
        yc = y + (h - b) `div` 2

centered' :: Rect -> Rect -> Rect
centered' r (Rect _ _ sz) = r `centered` sz
        
toRect :: Rect -> SDL.Rect
toRect (Rect x y (Size w h)) = SDL.Rect x y w h


renderLine :: String -> Rect -> Frontend u ()
renderLine line rc = do
    surf <- liftIO $ SDL.getVideoSurface
    fnt  <- liftM fromJust $ gets font
    text <- liftIO $ TTF.renderTextBlended fnt line cl
    liftIO $ do SDL.blitSurface text Nothing surf (Just $ toRect rc)
                SDL.freeSurface text
    return ()
  where cl = SDL.Color 255 255 255


renderString :: String -> Rect -> Frontend u ()
renderString str (Rect x y _) = do
  let ls = lines str
  sizes <- mapM getSDLLineSize ls
  let ys = scanl (+) y $ map snd sizes
  forM_ (zip3 ls ys sizes) $ \(l, y', (w, h)) ->
    renderLine l (Rect x y' (Size w h))
  
    
renderRect :: Rect -> (Int, Int, Int) -> Frontend u ()
renderRect rc (r, g, b) = do
  surf <- liftIO $ SDL.getVideoSurface
  let pixf = SDL.surfaceGetPixelFormat surf
  liftIO $ do cl <- SDL.mapRGB pixf
                    (fromIntegral r)
                    (fromIntegral g)
                    (fromIntegral b)
              SDL.fillRect surf (Just $ toRect rc) cl
  return ()

blue  = (0,   128, 255)
black = (0,     0,   0)
white = (255, 255, 255)

renderButton :: String -> WidgetState -> Rect -> Frontend u ()
renderButton s st rc = do
    when (st == Focused) $ renderRect rc blue
    sz <- textSize s
    renderString s (rc `centered` sz)


renderEdit :: String -> Int -> WidgetState -> Rect -> Frontend u ()
renderEdit text caret st rc = do
  let rc'  = centered' rc $ modRcSize rc $ \sz -> decrease sz 4 4
      rc'' = centered' rc $ modRcSize rc $ \sz -> decrease sz 7 7
      frameCl = if st == Focused then blue else white
  
  renderRect rc frameCl
  renderRect rc' black
  renderString text rc''
  
  when (st == Focused) $ do
    let ltext = take caret text
    (Size loffs _) <- textSize ltext
    let crc = Rect { rcX = rcX rc'' + loffs
                   , rcY = rcY rc''
                   , rcSize = Size 2 $ szHeight $ rcSize rc''
                   }
    renderRect crc white


renderSDL :: Widget i m -> WidgetState -> Rect -> Frontend u ()
renderSDL w st rc = case w of
  (Label s)    -> renderString s rc
  (Button s)   -> renderButton s st rc
  (Edit s c)   -> renderEdit s c st rc
  (Line i)     -> renderRect rc blue
  otherwise  -> return ()
  
  
endIterSDL :: Frontend u ()
endIterSDL = liftIO $ do
  surf <- SDL.getVideoSurface
  SDL.flip surf
  SDL.delay 30
  SDL.fillRect surf Nothing (SDL.Pixel 0)
  return ()

instance MonadFrontend (FrontendConfig u) (Frontend u) where
  initialize = initSDL
  getEvents = getSDLEvents
  render = renderSDL
  endIter = endIterSDL
  ownData = ownDataSDL
  runFcn = return runSDLFrontend

ownDataSDL :: Frontend u (FrontendConfig u)
ownDataSDL = get >>= return

getSDLLineSize :: String -> Frontend u (Int, Int)
getSDLLineSize line = do
  mfnt <- gets font
  liftIO $ TTF.textSize (fromJust mfnt) line
  
getSDLTextSize :: String -> Frontend u Size
getSDLTextSize str = do
  mfnt <- gets font
  sizes <- mapM getSDLLineSize $ lines str
  let width  = foldl' max 0 $ map fst sizes
      height = foldl' (+) 0 $ map snd sizes
  return $ Size width height


getSDLSurfSize :: Frontend u Size
getSDLSurfSize = return =<< gets size


instance MonadSurface (Frontend u) where
  textSize = getSDLTextSize
  surfSize = getSDLSurfSize


runSDLFrontend :: Frontend u a ->
                  FrontendConfig u ->
                  IO (a, FrontendConfig u)
runSDLFrontend (Frontend stt) sf = runStateT stt sf

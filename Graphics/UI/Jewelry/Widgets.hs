{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Graphics.UI.Jewelry.Widgets
       (
         jewelry
       , figureBox
       ) where

import Control.Monad (forM_)
import Control.Monad.State (gets)
import Control.Monad.Trans (liftIO)

import Data.Array (assocs)
import Data.Word (Word8)

import qualified Graphics.UI.SDL as SDL

import Graphics.UI.Oak (call)
import Graphics.UI.Oak.Basics
import Graphics.UI.Oak.Classes
import Graphics.UI.Oak.Widgets
import Graphics.UI.Oak.SDL

import Game.Jewelry
import Game.Jewelry.Basics
import Game.Jewelry.Field (Field, fieldMatrix)
import Game.Jewelry.Figure (Figure, figPoints, figJewels)


jewelry = Custom $ WidgetBehavior {
    accFocusFcn = jwAccFocus
  , sizePcyFcn  = jwSizePcy
  , sizeHintFcn = jwSizeHint
  , renderFcn   = jwRender
  }

jwAccFocus :: Bool
jwAccFocus = True

jwSizePcy :: Orientation -> (SizePolicy, SizePolicy)
jwSizePcy _ = (Fixed, Fixed)

jwSizeHint :: Monad m => Orientation -> m Size
jwSizeHint _ = return $ Size 180 390

jwRender :: WidgetState -> Rect -> Frontend Game ()
jwRender _ rc = do
     g <- gets userData
     dst <- liftIO $ SDL.getVideoSurface
     src <- liftIO $ SDL.createRGBSurface [] 180 390 32 0 0 0 0
     liftIO $ do renderField  src $ field g
                 renderFigure src $ figure g
                 SDL.blitSurface src Nothing dst (Just $ toRect rc)
                 SDL.freeSurface src
     return ()


renderField :: SDL.Surface -> Field -> IO ()
renderField surf f = forM_ cells $ renderCell surf
  where cells = assocs $ fieldMatrix f

renderFigure :: SDL.Surface -> Figure -> IO ()
renderFigure surf fig = forM_ cells (renderCell surf)
  where cells = filter visible
                $ zip (map ptData $ figPoints fig)
                      (map Jewel $ figJewels fig)

        ptData (Point pRow pCol) = (pRow, pCol)
        visible ((r, _), _) = r >= 1

renderCell :: SDL.Surface -> ((Int, Int), Cell) -> IO ()
renderCell surf ((row, col), cell) = do
    color <- SDL.mapRGB pixf clRed clGreen clBlue
    SDL.fillRect surf (Just $ SDL.Rect x y w h) color
    return ()
  where pixf = SDL.surfaceGetPixelFormat surf

        x = (col - 1) * w
        y = (row - 1) * h
        w = 30
        h = 30

        (clRed, clGreen, clBlue) = colorForCell cell

colorForCell :: Cell -> (Word8, Word8, Word8)
colorForCell Empty = (60, 60, 60)
colorForCell (Jewel j) =
  case j of
    Cherry -> (228,   0,  88)
    Green  -> ( 76, 220,  72)
    Blue   -> ( 60, 180, 252)
    Orange -> (240, 188,  60)
    Purple -> (149,  65, 252)
    Grape  -> (252, 116,  96)


figureBox = Custom $ WidgetBehavior {
    accFocusFcn   = False
  , sizePcyFcn  = const (Fixed, Fixed)
  , sizeHintFcn = const $ return $ Size 30 90
  , renderFcn   = fbRender
  }

fbRender :: WidgetState -> Rect -> Frontend Game ()
fbRender _ _ = return ()

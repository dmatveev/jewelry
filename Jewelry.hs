{-# LANGUAGE TemplateHaskell #-}

import Graphics.UI.SDL as SDL
import Data.Word(Word8, Word32)
import Data.Array (assocs)
import Data.List (foldl')
import Control.Monad (liftM, forM_)

import System.Time (ClockTime(..), getClockTime)

import Data.Mutators

import Basics
import Game

import Field (Field, fieldMatrix)
import Figure (Figure, figPoints, figJewels)
               
data DisplayContext = DisplayContext {
    surface   :: SDL.Surface
  , pixFormat :: SDL.PixelFormat
}

data AppState = AppState {
    display :: DisplayContext
  , game    :: GameState
}

genMutators ''AppState


currentSeconds :: IO Integer
currentSeconds = do
  (TOD sec _) <- getClockTime
  return sec


main :: IO ()
main = do
    SDL.init [SDL.InitEverything]
    SDL.enableKeyRepeat 1 1
    SDL.setVideoMode 640 480 32 []
    SDL.setCaption "Jewerly" "Jewerly"
    seed <- currentSeconds
    mainSurf <- SDL.getVideoSurface
    let pixFormat = SDL.surfaceGetPixelFormat mainSurf
    eventLoop $ AppState (DisplayContext mainSurf pixFormat)
                         (mkGame (13, 6) seed)
    SDL.quit


collectEvents :: IO [SDL.Event]
collectEvents = reverse `liftM` (collect' [])
    where collect' es = do
            event <- SDL.pollEvent
            if event == NoEvent
            then return es
            else collect' $ event : es


eventLoop :: AppState -> IO ()
eventLoop as = do
  evts <- collectEvents
  if quitPressed evts
  then return ()
  else do seconds <- currentSeconds
          let as' = as `modGame` \g -> setTicks g seconds
              newState = foldl' processEvent as' evts
          render newState
          SDL.delay 60
          eventLoop newState


processEvent :: AppState -> SDL.Event -> AppState
processEvent as (KeyDown k) = handleKbd as k
processEvent as _           = as


handleKbd :: AppState -> SDL.Keysym -> AppState
handleKbd as (SDL.Keysym k _ _) = modGame as $ \thisState ->
  case k of
    SDLK_LEFT  -> userMoveFigure ToLeft  thisState
    SDLK_RIGHT -> userMoveFigure ToRight thisState
    SDLK_DOWN  -> userMoveFigure ToDown  thisState
    SDLK_SPACE -> shuffleFigure thisState
    otherwise  -> thisState
          

quitPressed :: [SDL.Event] -> Bool
quitPressed evts = any (== SDL.Quit) evts


render :: AppState -> IO ()
render as = do
    let surf = surface dc
    SDL.fillRect surf Nothing (SDL.Pixel 0)

    renderField dc $ field g
    renderFigure dc $ figure g

    SDL.flip surf
 where dc = display as
       g = game as


renderField :: DisplayContext -> Field -> IO ()
renderField dc f = forM_ cells (renderCell dc)
  where cells = assocs $ fieldMatrix f


renderFigure :: DisplayContext -> Figure -> IO ()
renderFigure dc fig = forM_ cells (renderCell dc)
  where cells = zip (map ptData $ figPoints fig)
                    (map Jewel $ figJewels fig)

        ptData (Point pRow pCol) = (pRow, pCol)
          

renderCell :: DisplayContext -> ((Int, Int), Cell) -> IO ()
renderCell dc ((row, col), cell) = do
    color <- SDL.mapRGB pixf clRed clGreen clBlue
    SDL.fillRect surf (Just $ SDL.Rect x y w h) color
    return ()
  where surf = surface dc
        pixf = pixFormat dc
        x = (col - 1) * w
        y = (row - 1) * h
        w = 30
        h = 30

        (clRed, clGreen, clBlue) = colorForCell cell


colorForCell :: Cell -> (Word8, Word8, Word8)
colorForCell Empty = (60, 60, 60)
colorForCell (Jewel j) =
  case j of
    Red -> (255, 0, 0)
    Green -> (0, 255, 0)
    Blue -> (0, 0, 255)
    Yellow -> (255, 255, 0)
    Purple -> (160, 32, 240)
    White -> (255, 255, 255)
  
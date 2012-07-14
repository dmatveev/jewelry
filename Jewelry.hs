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
    SDL.setVideoMode 180 390 32 []
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


live :: Integer -> GameState -> GameState
live time gs =
  if time - ticks gs > 1
  then moveFigure ToDown $ setTicks gs time
  else gs


processEvents :: [SDL.Event] -> GameState -> GameState
processEvents evts gs = foldl' processEvent gs evts


processEvent :: GameState -> SDL.Event -> GameState
processEvent gs (KeyDown k) = handleKbd k gs
processEvent gs _           = gs


handleKbd :: SDL.Keysym -> GameState -> GameState
handleKbd (SDL.Keysym k _ _) gs = case k of
    SDLK_LEFT  -> moveFigure ToLeft  gs
    SDLK_RIGHT -> moveFigure ToRight gs
    SDLK_DOWN  -> shuffleFigure ToDown gs
    SDLK_UP    -> shuffleFigure ToUp gs
    SDLK_SPACE -> dropFigure gs
    otherwise  -> gs


eventLoop :: AppState -> IO ()
eventLoop as = do
  evts <- collectEvents
  if quitPressed evts
  then return ()
  else do seconds <- currentSeconds
          let newState = modGame as $ \g ->
                 processEvents evts $ live seconds g
          render newState
          SDL.delay 50
          eventLoop newState


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
  
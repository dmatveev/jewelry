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

data AppState = AppState {
    surface     :: SDL.Surface
  , game        :: Game
  , lastMoveKbd :: Word32
  , lastDropKbd :: Word32
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
    eventLoop $ AppState { surface = mainSurf
                         , game = mkGame (13, 6) seed
                         , lastMoveKbd = 0
                         , lastDropKbd = 0
                         }
    SDL.quit


collectEvents :: IO [SDL.Event]
collectEvents = reverse `liftM` (collect' [])
    where collect' es = do
            event <- SDL.pollEvent
            if event == NoEvent
            then return es
            else collect' $ event : es


live :: Integer -> Game -> Game
live time gs =
  if time - ticks gs > 1
  then moveFigure ToDown $ setTicks gs time
  else gs


processEvents :: Word32 -> [SDL.Event] -> AppState -> AppState
processEvents ticks evts as = foldl' (processEvent ticks) as evts


keepKbdSpeed :: Word32                           -- current time
             -> Word32                           -- desired interval
             -> (AppState -> Word32)             -- kbd time getter
             -> (AppState -> Word32 -> AppState) -- kbd time setter
             -> (AppState -> AppState)           -- app state action
             -> AppState                         -- initial app state
             -> AppState
keepKbdSpeed ticks interval getter setter f as =
  let elapsed = ticks - getter as
  in if elapsed > interval
     then f $ setter as ticks
     else as


processEvent :: Word32 -> AppState -> SDL.Event -> AppState
processEvent t as (KeyDown k) = handleKbd t k as
processEvent _ as _           = as


handleKbd :: Word32 -> SDL.Keysym -> AppState -> AppState
handleKbd ticks (SDL.Keysym k _ _) as = case k of
    SDLK_LEFT  -> defWard $ moveFigure ToLeft
    SDLK_RIGHT -> defWard $ moveFigure ToRight
    SDLK_DOWN  -> defWard $ shuffleFigure ToDown
    SDLK_UP    -> defWard $ shuffleFigure ToUp
    SDLK_SPACE -> spcWard $ dropFigure
    otherwise  -> as
  where
    wrapper f i s g = keepKbdSpeed ticks i s g (mutator f) as
    mutator f as = modGame as f
    defWard f = wrapper f 100 lastMoveKbd setLastMoveKbd
    spcWard f = wrapper f 500 lastDropKbd setLastDropKbd


eventLoop :: AppState -> IO ()
eventLoop as = do
  evts <- collectEvents
  if quitPressed evts
  then return ()
  else do seconds <- currentSeconds
          ticks <- SDL.getTicks   
          let newState = processEvents ticks evts $
                            modGame as $ \g -> live seconds g
          render newState
          SDL.delay 50
          eventLoop newState


quitPressed :: [SDL.Event] -> Bool
quitPressed evts = any (== SDL.Quit) evts


render :: AppState -> IO ()
render as = do
    SDL.fillRect surf Nothing (SDL.Pixel 0)

    if state g /= GameOver
    then do renderField surf $ field g
            renderFigure surf $ figure g
    else renderGameOver surf

    SDL.flip surf
 where (g, surf) = (game as, surface as)
       fld = field g


renderGameOver :: SDL.Surface -> IO ()
renderGameOver surf = do
    SDL.fillRect surf Nothing (Pixel 0)
    return ()
                      

renderField :: SDL.Surface -> Field -> IO ()
renderField surf f = forM_ cells (renderCell surf)
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
  
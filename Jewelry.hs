import Graphics.UI.SDL as SDL
import Data.Word(Word32)
import Control.Monad (liftM)

import Game

data DisplayContext = DisplayContext {
    surface   :: SDL.Surface
  , pixFormat :: SDL.PixelFormat
}

main :: IO ()
main = do
    SDL.init [SDL.InitEverything]
    SDL.enableKeyRepeat 1 1
    SDL.setVideoMode 640 480 32 []
    SDL.setCaption "Jewerly" "Jewerly"
    mainSurf <- SDL.getVideoSurface
    let pixFormat = SDL.surfaceGetPixelFormat mainSurf
    eventLoop $ DisplayContext mainSurf pixFormat
    SDL.quit

collectEvents :: IO [SDL.Event]
collectEvents = reverse `liftM` (collect' [])
    where collect' es = do
            event <- SDL.pollEvent
            if event == NoEvent
            then return es
            else collect' $ event : es

eventLoop :: DisplayContext -> IO ()
eventLoop dc = do
  evts <- collectEvents
  if quitPressed evts
  then return ()
  else do let surf = surface dc
          SDL.fillRect surf Nothing (SDL.Pixel 0)
          SDL.delay 30
          SDL.flip surf
          eventLoop dc

quitPressed :: [SDL.Event] -> Bool
quitPressed evts = any (== SDL.Quit) evts

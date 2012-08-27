{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

import Control.Monad (liftM, forM_)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Array (assocs)
import Data.List (foldl')
import Data.Mutators
import Data.Word (Word8, Word32)


import Graphics.UI.Oak
import Graphics.UI.Oak.Basics
import Graphics.UI.Oak.Classes
import Graphics.UI.Oak.Widgets
import Graphics.UI.Oak.Utils (currentSeconds)
import Graphics.UI.Oak.SDL

import Graphics.UI.Jewelry.Screens
import Graphics.UI.Jewelry.Widgets

import Game.Jewelry
import Game.Jewelry.Basics
import Game.Jewelry.Field (Field, fieldMatrix)
import Game.Jewelry.Figure (Figure, figPoints, figJewels)


clicked :: MonadHandler WidgetId mh m => String -> m ()
clicked s = liftIO $ putStrLn $ s ++ " clicked"
                  

handlers = [ (BtnPlay,  KeyDown Return, open gameScreen)
           , (BtnFame,  KeyDown Return, open hiscoresScreen)
           , (BtnAbout, KeyDown Return, open aboutScreen)
           , (BtnQuit,  KeyDown Return, quit)

           , (BtnBack,  KeyDown Return, back)
           ]


main :: IO ()
main = do
    seed <- currentSeconds
    let game = mkGame (13, 6) seed
        conf = fConfig
               "Jewelry"
               (Font "LiberationSans-Bold.ttf" 20)
               (Size 640 480)
               game
               
    runSDLFrontend (runOak mainScreen hs) conf >> return ()
  where hs = handlers ++ bindHandlers Jewelry jwHandlers


-- keepKbdSpeed :: Word32                           -- current time
--              -> Word32                           -- desired interval
--              -> (AppState -> Word32)             -- kbd time getter
--              -> (AppState -> Word32 -> AppState) -- kbd time setter
--              -> (AppState -> AppState)           -- app state action
--              -> AppState                         -- initial app state
--              -> AppState
-- keepKbdSpeed ticks interval getter setter f as =
--   let elapsed = ticks - getter as
--   in if elapsed > interval
--      then f $ setter as ticks
--      else as


-- handleKbd :: Word32 -> SDL.Keysym -> AppState -> AppState
-- handleKbd ticks (SDL.Keysym k _ _) as = case k of
--     SDLK_LEFT  -> defWard $ moveFigure ToLeft
--     SDLK_RIGHT -> defWard $ moveFigure ToRight
--     SDLK_DOWN  -> defWard $ shuffleFigure ToDown
--     SDLK_UP    -> defWard $ shuffleFigure ToUp
--     SDLK_SPACE -> spcWard $ dropFigure
--     otherwise  -> as
--   where
--     wrapper f i s g = keepKbdSpeed ticks i s g (mutator f) as
--     mutator f as = modGame as f
--     defWard f = wrapper f 100 lastMoveKbd setLastMoveKbd
--     spcWard f = wrapper f 500 lastDropKbd setLastDropKbd

{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

import Control.Monad (liftM, forM_, when)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State (modify, gets)
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
                  

handlers = [ (BtnPlay,  KeyDown Return, playNewGame)
           , (BtnFame,  KeyDown Return, open hiscoresScreen)
           , (BtnAbout, KeyDown Return, open aboutScreen)
           , (BtnQuit,  KeyDown Return, tryQuit)

           , (BtnBack,  KeyDown Return, back)

           , (Jewelry, KeyDown ArrowLeft,  jwMove ToLeft)
           , (Jewelry, KeyDown ArrowRight, jwMove ToRight)
           , (Jewelry, KeyDown ArrowDown,  jwShuffle ToDown)
           , (Jewelry, KeyDown ArrowUp,    jwShuffle ToUp)
           , (Jewelry, KeyDown SpaceKey,   jwDropFigure)
           , (Jewelry, KeyDown F10,        jwEndGame)
           , (Jewelry, Live,               jwLive)
           ]

goHandlers = [(BtnBack, KeyDown Return, answer ())]

tryQuit :: MonadHandler WidgetId () (Frontend Game) m => m ()
tryQuit = do
    ma <- hlift $ msgBox title text [No, Yes]
    maybe (return ()) (\a -> when (a == Yes) quit) ma
  where title = "Quit"
        text  = "Are you sure you want to quit?"


playNewGame :: MonadHandler WidgetId () (Frontend Game) m => m ()
playNewGame = do
  seed <- liftIO $ currentSeconds
  hlift $ modify $ \s -> setUserData s $ mkGame (13, 6) seed
  open gameScreen


jwMove :: MonadHandler i () (Frontend Game) m => Direction -> m ()
jwMove d = hlift $ modify $ \s -> modUserData s $ moveFigure d

jwShuffle :: MonadHandler i () (Frontend Game) m => Direction -> m ()
jwShuffle d = hlift $ modify $ \s -> modUserData s $ shuffleFigure d

jwDropFigure :: MonadHandler i () (Frontend Game) m => m ()
jwDropFigure = hlift $ modify $ \s -> modUserData s dropFigure

jwEndGame :: (Identifier i, MonadHandler i () (Frontend Game) m)
             => m ()
jwEndGame = do
  ma <- hlift $ msgBox
        "End game"
        "Are you sure you want to end this game?"
        [No, Yes]
  maybe (return ()) (\a -> when (a == Yes) back) ma
  

jwLive :: (Identifier i, MonadHandler i () (Frontend Game) m)
          => m ()
jwLive = do
  st <- hlift $ gets (state . userData)
  if st == GameOver
    then (hlift $ call gameOverScreen goHandlers) >> back
    else do t <- now
            hlift $ modify $ \s -> modUserData s $ \g ->
              if t - ticks g > 1
              then moveFigure ToDown $ setTicks g t
              else g


main :: IO ()
main = do
    seed <- currentSeconds
    let game = mkGame (13, 6) seed
        conf = fConfig
               "Jewelry"
               (Font "LiberationSans-Bold.ttf" 20)
               (Size 640 480)
               game
               
    runSDLFrontend (runOak mainScreen handlers) conf >> return ()


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

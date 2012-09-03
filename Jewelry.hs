{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

import Data.Array (assocs)
import Data.List (foldl')
import Data.Mutators
import Data.Word (Word8, Word32)

import Control.Monad (liftM, forM_, when)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State (modify, gets)

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
import Game.Jewelry.HighScore


handlers = [ (BtnPlay,  KeyDown Return, playNewGame)
           , (BtnFame,  KeyDown Return, openHighScores)
           , (BtnAbout, KeyDown Return, openAboutScreen)
           , (BtnQuit,  KeyDown Return, tryQuit)

           , (BtnBack,  KeyDown Return, back)
           ]

jwHandlers = [ (Jewelry, KeyDown ArrowLeft,       alteringGame $ jwMove ToLeft)
             , (Jewelry, KeyDown ArrowRight,      alteringGame $ jwMove ToRight)
             , (Jewelry, KeyDown ArrowDown,       jwShuffle ToDown)
             , (Jewelry, KeyDown ArrowUp,         jwShuffle ToUp)
             , (Jewelry, KeyDown SpaceKey,        alteringGame $ jwDropFigure)
             , (Jewelry, KeyDown (Character 'p'), jwPauseGame)
             , (Jewelry, KeyDown F10,             jwEndGame)
             , (Jewelry, Live,                    jwLive)
             ]


tryQuit :: MonadHandler WidgetId () (Frontend Game) m => m ()
tryQuit = do
    ma <- msgBox title text [No, Yes]
    maybe (return ()) (\a -> when (a == Yes) quit) ma
  where title = "Quit"
        text  = "Are you sure you want to quit?"

playNewGame :: MonadHandler WidgetId () (Frontend Game) m => m ()
playNewGame = do
    seed <- liftIO $ currentSeconds
    hlift $ modify $ \s -> setUserData s $ mkGame (13, 6) seed
    mgr <- hlift $ call gameScreen jwHandlers
    maybe (return ()) checkHighScore mgr
  where checkHighScore gr = do
          hsc <- hlift $ gets (hiscore . userData)
          when (isHighScore Classic gr hsc) $ do
            mname <- inputBox
                     "Congratulations!"
                     "You have a highcore! Please enter your name:"
            maybe (return ()) (storeHighScore hsc gr) mname

        storeHighScore hsc gr name = do
          hlift $ modify $ \s ->
            modUserData s $ \g ->
            setHiscore g $
            putScore Classic name gr hsc


openHighScores :: MonadHandler WidgetId () (Frontend Game) m => m ()
openHighScores = do
    hsc <- hlift $ gets (hiscore . userData)
    liftIO $ putStrLn $ show hsc

openAboutScreen :: MonadHandler WidgetId () (Frontend Game) m => m ()
openAboutScreen = open aboutScreen
          
jwMove :: MonadHandler i GameResult (Frontend Game) m => Direction -> m ()
jwMove d = hlift $ modify $ \s -> modUserData s $ moveFigure d

jwShuffle :: MonadHandler i GameResult (Frontend Game) m => Direction -> m ()
jwShuffle d = hlift $ modify $ \s -> modUserData s $ shuffleFigure d

jwDropFigure :: MonadHandler i GameResult (Frontend Game) m => m ()
jwDropFigure = hlift $ modify $ \s -> modUserData s dropFigure

pausingGame :: MonadHandler WidgetId GameResult (Frontend Game) m => m a -> m a
pausingGame act = do
  r <- act
  t <- now
  hlift $ modify $ \s -> modUserData s $ \g -> setTicks g t
  return r
  

jwPauseGame :: MonadHandler WidgetId GameResult (Frontend Game) m => m ()
jwPauseGame = pausingGame $ 
              msgBox "Jewelry" "Game paused" [Ok]
              >> return ()

jwEndGame :: MonadHandler WidgetId GameResult (Frontend Game) m
             => m ()
jwEndGame = do
  ma <- pausingGame $ msgBox
        "End game"
        "Are you sure you want to end this game?"
        [No, Yes]
  maybe (return ()) (\a -> when (a == Yes) quit) ma
  

alteringGame :: MonadHandler WidgetId GameResult (Frontend Game) m
                => m () -> m ()
alteringGame act = do
    rBefore <- acquireResult
    act
    rAfter <- acquireResult
    updateInfoTable Score   "Score: "   totalScore   rBefore rAfter
    updateInfoTable Figures "Figures: " totalFigures rBefore rAfter

  where
    acquireResult = hlift $ gets (result . userData)
    
    updateInfoTable i t acc before after = do
      let current = acc after
      when (acc before /= current) $ alter i $ \_ -> Label $ t ++ (show current)
  


jwLive :: MonadHandler WidgetId GameResult (Frontend Game) m
          => m ()
jwLive = do
    gm <- hlift $ gets userData
    if state gm == GameOver
      then do msgBox "Jewelry" "Game over" [Ok]
              answer $ result gm
      else alteringGame $ do
              t <- now
              hlift $ modify $ \s -> modUserData s $ \g ->
                if t - ticks g > 1
                then moveFigure ToDown $ setTicks g t
                else g

main :: IO ()
main = do
    seed <- currentSeconds
    let conf = fConfig
               "Jewelry"
               (Font "LiberationSans-Bold.ttf" 20)
               (Size 640 480)
               (mkGame (13, 6) seed)
               
    runSDLFrontend (initOak mainScreen handlers) conf
    return ()

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

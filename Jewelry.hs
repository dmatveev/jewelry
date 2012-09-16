{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

import Data.Array (assocs)
import Data.List (foldl')
import Data.Maybe (fromJust)
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


scoreFile = "scores"

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

checkHighScore :: MonadHandler WidgetId () (Frontend Game) m =>
                  GameResult -> m ()
checkHighScore gr = do
    hsc <- hlift $ gets (hiscore . userData)
    when (isHighScore Classic gr hsc) $ do
      mname <- inputBox
               "Congratulations!"
               "You have a highcore! Please enter your name:"
      hlift $ maybe (return ()) (storeHighScore gr) mname
      openHighScores


storeHighScore :: GameResult -> String -> Frontend Game ()
storeHighScore gr name = do
  modify $ \s ->
    modUserData s $ \g ->
    modHiscore g $ putScore Classic name gr
  sc <- gets (hiscore . userData)
  liftIO $ storeScore sc scoreFile

  
playNewGame :: MonadHandler WidgetId () (Frontend Game) m => m ()
playNewGame = do
    seed <- liftIO $ currentSeconds
    hscs <- hlift $ gets (hiscore . userData)
    hlift $ modify $ \s ->
      setUserData s $ setHiscore (mkGame (13, 6) seed) hscs
    mgr <- hlift $ call gameScreen jwHandlers
    maybe (return ()) checkHighScore mgr


openHighScores :: MonadHandler WidgetId () (Frontend Game) m => m ()
openHighScores = (hlift $ gets (hiscore . userData))
                 >>= (open . hiscoresScreen)

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
jwPauseGame = pausingGame $ msgBox "Jewelry" "Game paused" [Ok]
              >> return ()

jwEndGame :: MonadHandler WidgetId GameResult (Frontend Game) m
             => m ()
jwEndGame = do
    ma <- pausingGame $ msgBox
          "End game"
          "Are you sure you want to end this game?"
          [No, Yes]
    maybe (return ()) (\a -> when (a == Yes) quitGame) ma
  where quitGame = (hlift $ gets (result . userData)) >>= answer 
  

alteringGame :: MonadHandler WidgetId GameResult (Frontend Game) m
                => m () -> m ()
alteringGame act = do
    rBefore <- acquireResult
    act
    rAfter <- acquireResult
    updateInfoTable Score   totalScore   rBefore rAfter
    updateInfoTable Figures totalFigures rBefore rAfter

  where
    acquireResult = hlift $ gets (result . userData)
    
    updateInfoTable i acc before after = do
      let current = acc after
      when (acc before /= current) $ alter i $ \_ -> Label $ show current
  

jwLive :: MonadHandler WidgetId GameResult (Frontend Game) m => m ()
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
    score <- loadScore scoreFile
      
    let game = mkGame (13, 6) seed
        conf = fConfig
               "Jewelry"
               (Font "LiberationSans-Bold.ttf" 20)
               (Size 640 480)
               (maybe game (setHiscore game) score)
               
    runSDLFrontend (runOak mainScreen handlers) conf
    return ()

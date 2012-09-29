{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Jewelry (playNewGame, handlers, jwHandlers) where

import Data.Time.Clock

import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)

import Graphics.UI.Oak
import Graphics.UI.Oak.Basics
import Graphics.UI.Oak.Classes
import Graphics.UI.Oak.Widgets
import Graphics.UI.Oak.SDL

import Graphics.UI.Jewelry.Screens
import Graphics.UI.Jewelry.Widgets

import Game.Jewelry
import Game.Jewelry.Basics
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
             , (Jewelry, KeyDown (Character 'P'), jwPauseGame)
             , (Jewelry, KeyDown F10,             jwEndGame)
             , (Jewelry, Live,                    jwLive)
             ]


tryQuit :: (MonadUserState Game mus, MonadHandler WidgetId () mus m) => m ()
tryQuit = do
    ma <- msgBox title text [No, Yes]
    maybe (return ()) (\a -> when (a == Yes) quit) ma
  where title = "Quit"
        text  = "Are you sure you want to quit?"

checkHighScore :: (MonadUserState Game mus, MonadIO mus,
                   MonadHandler WidgetId () mus m) =>
                  GameResult -> m ()
checkHighScore gr = do
    hsc <- hlift $ usGets hiscore
    when (isHighScore Classic gr hsc) $ do
      mname <- inputBox
               "Congratulations!"
               "You have a highscore! Please enter your name:"
      hlift $ maybe (return ()) (storeHighScore gr) mname
      openHighScores


storeHighScore :: (MonadUserState Game mus, MonadIO mus) => GameResult -> String -> mus ()
storeHighScore gr name = do
  usMod $ \g -> modHiscore g $ putScore Classic name gr
  sc <- usGets hiscore
  liftIO $ storeScore sc

  
-- Yes, this funciton has no prototype.
-- I do not know how to express it using only monadic interface constraints.
-- My Haskell fu is not strong enough.
playNewGame = do
    seed <- liftIO $ getCurrentTime
    hscs <- hlift $ usGets hiscore
    hlift $ usPut $ setHiscore (mkGame (13, 6) seed) hscs
    mgr <- hlift $ call gameScreen jwHandlers
    maybe (return ()) checkHighScore mgr


openHighScores :: (MonadUserState Game mus, MonadHandler WidgetId () mus m) => m ()
openHighScores = (hlift $ usGets hiscore) >>= (open . hiscoresScreen)

openAboutScreen :: (MonadUserState Game mus, MonadHandler WidgetId () mus m) => m ()
openAboutScreen = open aboutScreen
          
jwMove :: (MonadUserState Game mus, MonadHandler i GameResult mus m) => Direction -> m ()
jwMove d = hlift $ usMod $ moveFigure d

jwShuffle :: (MonadUserState Game mus, MonadHandler i GameResult mus m) => Direction -> m ()
jwShuffle d = hlift $ usMod $ shuffleFigure d

jwDropFigure :: (MonadUserState Game mus, MonadHandler i GameResult mus m) => m ()
jwDropFigure = do
  t <- liftIO getCurrentTime
  hlift $ usMod $ \g -> (dropFigure $ setTicks g t)

pausingGame :: (MonadUserState Game mus, MonadHandler WidgetId GameResult mus m) => m a -> m a
pausingGame act = do
  r <- act
  t <- liftIO getCurrentTime
  hlift $ usMod $ \g -> setTicks g t
  return r
  
jwPauseGame :: (MonadUserState Game mus, MonadHandler WidgetId GameResult mus m) => m ()
jwPauseGame = pausingGame $ msgBox "Jewelry" "Game paused" [Ok] >> return ()

jwEndGame :: (MonadUserState Game mus, MonadHandler WidgetId GameResult mus m) => m ()
jwEndGame = do
    ma <- pausingGame $ msgBox
          "End game"
          "Are you sure you want to end this game?"
          [No, Yes]
    maybe (return ()) (\a -> when (a == Yes) quitGame) ma
  where quitGame = (hlift $ usGets result) >>= answer 
  

alteringGame :: (MonadUserState Game mus, MonadHandler WidgetId GameResult mus m) => m () -> m ()
alteringGame act = do
    rBefore <- acquireResult
    act
    rAfter <- acquireResult
    updateInfoTable Score   totalScore   rBefore rAfter
    updateInfoTable Level   level        rBefore rAfter
    updateInfoTable Figures totalFigures rBefore rAfter

  where
    acquireResult = hlift $ usGets result
    
    updateInfoTable i acc before after = do
      let current = acc after
      when (acc before /= current) $ alter i $ \_ -> Label $ show current
  

timeDiffD :: UTCTime -> UTCTime -> Double
timeDiffD t1 t2 = fromRational $ toRational $ diffUTCTime t1 t2

ticksFor :: Integer -> Double
ticksFor l = max 0.2 tfl
  where tfl = 1.0 - (fromInteger l) * 0.05

jwLive :: (MonadUserState Game mus, MonadHandler WidgetId GameResult mus m) => m ()
jwLive = do
  gm <- hlift usGet
  if state gm == GameOver
    then msgBox "Jewelry" "Game over" [Ok] >> answer (result gm)
    else alteringGame $ do
      t <- liftIO getCurrentTime
      hlift $ usMod $ \g ->
        if timeDiffD t (ticks g) > (ticksFor $ level $ result g)
        then moveFigure ToDown $ setTicks g t
        else g


main :: IO ()
main = do
    seed <- getCurrentTime
    score <- loadScore
      
    let game = mkGame (13, 6) seed
        conf = fConfig
               "Jewelry"
               (Font "LiberationSans-Bold.ttf" 20)
               (Size 640 480)
               (maybe game (setHiscore game) score)
               
    runSDLFrontend (runOak mainScreen handlers) conf
    return ()

module Graphics.UI.Jewelry.Screens
       (
         WidgetId(..)
         
       , mainScreen
       , gameScreen
       , gameOverScreen
       , hiscoresScreen
       , aboutScreen
       ) where


import Graphics.UI.Oak.Classes
import Graphics.UI.Oak.Widgets

import Graphics.UI.Jewelry.Widgets (jewelry, figureBox)

data WidgetId = BtnPlay | BtnFame | BtnAbout | BtnQuit | BtnBack
              | Jewelry
              | Score
              | NoName
                deriving (Eq, Show)

instance Identifier WidgetId where
  unused  = NoName
  btnBack = BtnBack


mainScreen = center (NoName, mainMenu)
 where mainMenu = vbox [ (BtnPlay,  Button "Play")
                       , (BtnFame,  Button "Highscores")
                       , (BtnAbout, Button "About")
                       , (BtnQuit,  Button "Quit")
                       ]
              

gameScreen = hbox [ (NoName, center (Jewelry, jewelry))
                  , (NoName, Line 3)
                  , (NoName, sideBar)
                  ]
  where sideBar = vbox [ (NoName, nextFigure)
                       , (NoName, Line 3)
                       , (NoName, scoreTable)
                       , (NoName, Line 3)
                       , (NoName, Label keyHints)
                       ]
        nextFigure = Compact $
                     vbox [ (NoName, Label "Next figure:")
                          , (NoName, center (NoName, figureBox))
                          ]
        scoreTable = Compact $
                     vbox [ (Score,  Label "Score:")
                          , (NoName, Label "Level:")
                          , (NoName, Label "Figures:")
                          , (NoName, Stretch)
                          ]
        keyHints = unlines
                   [ "Keyboard hints:"
                   , "Arrow keys: move figure"
                   , "Space: drop figure"
                   , "P: pause"
                   , "F10: end game"
                   ]

bback = (BtnBack, Button "Back")

gameOverScreen = dialog "Game over" [bback] $ (NoName, Stretch)


hiscoresScreen = dialog "Highscores" [bback] $ (NoName, Stretch)


aboutScreen = dialog "About" [bback] $
              (NoName, center (NoName, Label text))
  where text = unlines
               [ "Written by Dmitry Matveev"
               , "Released under terms of the MIT license"
               , "http://github.com/dmatveev/jewelry"
               ]

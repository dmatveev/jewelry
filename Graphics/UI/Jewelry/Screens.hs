module Graphics.UI.Jewelry.Screens
       (
         WidgetId(..)
         
       , mainScreen
       , gameScreen
       , hiscoresScreen
       , aboutScreen
       ) where


import Graphics.UI.Oak.Classes
import Graphics.UI.Oak.Widgets

import Graphics.UI.Jewelry.Widgets (jewelry, figureBox)

data WidgetId = BtnPlay | BtnFame | BtnAbout | BtnQuit | BtnBack
              | Jewelry
              | NoName
                deriving (Eq, Show)

instance Identifier WidgetId where
  unused = NoName

instance DialogIdentifier WidgetId where
  backBtn = BtnBack


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
                     vbox [ (NoName, Label "Score:")
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


hiscoresScreen = dialog "Highscores" $ (NoName, Stretch)


aboutScreen = dialog "About" $ (NoName, center (NoName, Label text))
  where text = unlines
               [ "Written by Dmitry Matveev"
               , "Released under terms of the MIT license"
               , "http://github.com/dmatveev/jewelry"
               ]

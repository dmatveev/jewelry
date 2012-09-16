module Graphics.UI.Jewelry.Screens
       (
         WidgetId(..)
         
       , mainScreen
       , gameScreen
       , hiscoresScreen
       , aboutScreen
       ) where

import Data.Maybe (fromJust)
import Data.List (intersperse)
import Control.Applicative ((<$>))

import Graphics.UI.Oak.Classes
import Graphics.UI.Oak.Widgets

import Game.Jewelry.Basics
import Game.Jewelry.HighScore


import Graphics.UI.Jewelry.Widgets (jewelry, figureBox)

data WidgetId = BtnPlay | BtnFame | BtnAbout | BtnQuit | BtnBack
              | EdtName
              | Jewelry | Score | Figures
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
  where sideBar = vgrid [nextFigure, scoreTable, keyHints]
        nextFigure = compact $ vbox
                     [ (NoName, Label "Next figure:")
                     , (NoName, center (NoName, figureBox))
                     ]
        scoreTable = compact $ table
                     [ [(NoName,  Label "Score:"),   (Score,   Label "0")]
                     , [(NoName,  Label "Level:"),   (NoName,  Label "?")]
                     , [(NoName,  Label "Figures:"), (Figures, Label "0")]
                     , exprow
                     ]
        keyHints = compact $ table $ labelize
                   [ ["Arrow keys:", "move figure"]
                   , ["Space:",      "drop figure"]
                   , ["P:",          "pause"]
                   , ["F10:",        "end game"]
                   ]

        exprow      = [(NoName,  Stretch), (NoName,  Stretch)]
        vgrid       = vbox . lined . map (\w -> (NoName, margin 10 $ (NoName, w)))
        labelize ss = (map (map (\s -> (NoName, Label s))) ss) ++ [exprow]
        lined       = intersperse (NoName, Line 3)

bback = (BtnBack, Button "Back")

hiscoresScreen hsc =
    dialog "Highscores" [bback] $
    (NoName, vbox [ (NoName, compact $ table $ labels : hscws hsc)
                  , (NoName, Stretch)
                  ]
    )
  where labels = spaced $ mkLabels ["Name", "Score", "Figures"]
        mkLabels ss = (fmap hexpand $ mkLabel $ head ss) : map mkLabel (tail ss)
        mkLabel a   = (NoName, Label a)

        hscws h = map mkRow $ fromJust $ lookup Classic $ scoreEntries h
        mkRow f = spaced $ map mkLabel [ player f
                                       , (show $ totalScore   $ results f)
                                       , (show $ totalFigures $ results f)
                                       ]

        spaced = intersperse (NoName, Space 10)


aboutScreen = dialog "About" [bback] $
              (NoName, center (NoName, Label text))
  where text = unlines
               [ "Written by Dmitry Matveev"
               , "Released under terms of the MIT license"
               , "http://github.com/dmatveev/jewelry"
               ]

module Game.Jewelry.HighScore
       (
         FameEntry(..)
       , HighScore
       , highscore
       , isHighScore
       , scoreEntries
       , putScore
       ) where

import Data.List (sortBy, foldl)
import Data.Maybe (maybe)

import Game.Jewelry.Basics (GameResult(..), DifficultyLevel(..))


data FameEntry = FameEntry {
    player  :: String
  , results :: GameResult
  } deriving (Eq, Show)

newtype HighScore = HighScore {
  scoreEntries :: [(DifficultyLevel, [FameEntry])]
  } deriving (Eq, Show)

highscore :: HighScore
highscore = HighScore [ (Classic, [])
                      , (Medium,  [])
                      , (Easy,    [])
                      ]

tableSize :: Int
tableSize = 3

isHighScore :: DifficultyLevel ->
               GameResult ->
               HighScore ->
               Bool
isHighScore d gr (HighScore hs) =
    if totalScore gr == 0
    then False
    else maybe False cmp $ lookup d hs
  where cmp fes
          | null fes  = True
          | otherwise = totalScore gr > totalScore (results $ last fes)

sorted :: [FameEntry] -> [FameEntry]
sorted fes = sortBy score fes
  where score fe1 fe2 = compare
                        (totalScore $ results fe1)
                        (totalScore $ results fe2)

modScore :: FameEntry -> [FameEntry] -> [FameEntry]
modScore e fes = take tableSize . sorted $ e : fes

putScore :: DifficultyLevel ->
            String ->
            GameResult ->
            HighScore ->
            HighScore
putScore lvl name res (HighScore hs) = HighScore $ map upd hs
  where upd p@(l, es) = if l == lvl
                        then (l, modScore e es)
                        else p
        e = FameEntry name res

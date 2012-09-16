module Game.Jewelry.HighScore
       (
         FameEntry(..)
       , HighScore
       , highscore
       , isHighScore
       , scoreEntries
       , putScore

       , loadScore
       , storeScore
       ) where

import Data.Maybe (maybe)
import Data.List (sortBy, foldl)
import System.IO (hSetBinaryMode, withFile, IOMode(..))
import Control.Exception (handle, evaluate, SomeException)
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Codec.Compression.Zlib as Z


import Game.Jewelry.Basics (GameResult(..), DifficultyLevel(..))


data FameEntry = FameEntry {
    player  :: String
  , results :: GameResult
  } deriving (Eq, Show, Read)

newtype HighScore = HighScore {
  scoreEntries :: [(DifficultyLevel, [FameEntry])]
  } deriving (Eq, Show, Read)

highscore :: HighScore
highscore = HighScore [ (Classic, [])
                      , (Medium,  [])
                      , (Easy,    [])
                      ]

tableSize :: Int
tableSize = 10

isHighScore :: DifficultyLevel ->
               GameResult ->
               HighScore ->
               Bool
isHighScore d gr (HighScore hs) =
    if totalScore gr == 0
    then False
    else maybe False cmp $ lookup d hs
  where cmp fes
          | null fes || length fes < tableSize = True
          | otherwise = totalScore gr > totalScore (results $ last fes)

sorted :: [FameEntry] -> [FameEntry]
sorted fes = reverse $ sortBy score fes
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


fallback :: IO a -> SomeException -> IO a
fallback a _ = a


loadScore :: String -> IO (Maybe HighScore)
loadScore file = handle (fallback $ return Nothing) $ do
  withFile file ReadMode $ \h -> do
    hSetBinaryMode h True 
    z <- B.hGetContents h
    liftM Just $ evaluate $ read $ B.unpack $ Z.decompress z


storeScore :: HighScore -> String -> IO ()
storeScore hs file = handle (fallback $ return ()) $ do
  withFile file WriteMode $ \h ->
    B.hPut h $ Z.compress $ B.pack $ show hs

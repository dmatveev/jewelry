module Game.Jewelry.Basics where

data Jewel = Cherry
           | Green
           | Blue
           | Grape
           | Purple
           | Orange
             deriving (Eq, Show)

data Cell = Jewel Jewel
          | Empty
            deriving (Eq, Show)

data Direction = ToLeft
               | ToRight
               | ToDown
               | ToUp
                 deriving (Eq, Show)

data Point = Point Int Int
             deriving (Eq, Show)

data DifficultyLevel = Classic | Medium | Easy
                       deriving (Eq, Show)

data GameResult = GameResult {
    totalScore   :: Integer
  , totalFigures :: Integer
  } deriving (Eq, Show)

gameResult :: GameResult
gameResult = GameResult {
    totalScore = 0
  , totalFigures = 0
  }

class GameObject a where
  moveInto :: a -> Point -> a
  moveTo   :: a -> Direction -> a
  position :: a -> Point


evolve :: (a -> a) -> a -> [a]
evolve f x = x : evolve f (f x)


frepeat :: (a -> a) -> a -> Integer -> a
frepeat _ a 0 = a
frepeat f a n = frepeat f (f a) (n - 1)

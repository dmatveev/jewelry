module Basics where

data Jewel = Red
           | Green
           | Blue
           | Yellow
           | Purple
           | White
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

class GameObject a where
  moveInto :: a -> Point -> a
  moveTo   :: a -> Direction -> a
  position :: a -> Point


evolve :: (a -> a) -> a -> [a]
evolve f x = x : evolve f (f x)


frepeat :: (a -> a) -> a -> Integer -> a
frepeat _ a 0 = a
frepeat f a n = frepeat f (f a) (n - 1)

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
                 deriving (Eq, Show)

data Point = Point Int Int
             deriving (Eq, Show)

class GameObject a where
  moveInto :: a -> Point -> a
  moveTo   :: a -> Direction -> a
  position :: a -> Point

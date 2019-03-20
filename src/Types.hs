module Types where

data Program = Program
  { name :: String
  , timeSlot :: String
  } deriving (Show)

data Channel = Channel
  { channel :: String
  , programming :: [Program]
  } deriving (Show)

data Day = Day
  { day :: String
  , channels :: [Channel]
  } deriving (Show)

newtype Week = Week [Day] deriving (Show)

module Types where

import Data.Text (Text)

data Program = Program
  { name :: Text
  , timeSlot :: Text
  }

data Channel = Channel
  { channel :: Text
  , programming :: [Program]
  }

data Day = Day
  { day :: Text
  , channels :: [Channel]
  }

newtype Week = Week [Day]

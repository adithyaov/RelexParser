{-# LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics
import Data.Aeson

data Program = Program
  { name :: String
  , timeslot :: String
  } deriving (Show, Generic)

data Channel = Channel
  { channel :: String
  , programming :: [Program]
  } deriving (Show, Generic)

data Day = Day
  { day :: String
  , channels :: [Channel]
  } deriving (Show, Generic)

newtype Week = Week [Day] deriving (Show, Generic)

instance ToJSON Program
instance ToJSON Channel
instance ToJSON Day
instance ToJSON Week

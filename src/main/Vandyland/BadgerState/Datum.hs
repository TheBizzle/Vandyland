{-# LANGUAGE DeriveGeneric #-}
module Vandyland.BadgerState.Datum(Datum(..)) where

import Data.Aeson(ToJSON)

import GHC.Generics(Generic)

data Datum
  = Datum {
      value     :: Text
    , timestamp :: Integer
    } deriving (Generic, Show)

instance ToJSON Datum

{-# LANGUAGE DeriveGeneric #-}
module Submission(Submission(..)) where

import Bizzlelude

import Data.Aeson(ToJSON)

import GHC.Generics(Generic)

data Submission
  = Submission {
      uploadName  :: Text
    , base64Image :: Text
    } deriving (Generic, Show)

instance ToJSON Submission

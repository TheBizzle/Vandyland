{-# LANGUAGE DeriveGeneric #-}
module Submission(Submission(..)) where

import Data.Aeson(ToJSON)

import GHC.Generics(Generic)

data Submission
  = Submission {
      uploadName  :: Text
    , base64Image :: Text
    , metadata    :: Maybe Text
    } deriving (Generic, Show)

instance ToJSON Submission

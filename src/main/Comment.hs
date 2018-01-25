{-# LANGUAGE DeriveGeneric #-}
module Comment(Comment(..)) where

import Data.Aeson(ToJSON)

import GHC.Generics(Generic)

data Comment
  = Comment {
      uuid    :: Text
    , comment :: Text
    , author  :: Text
    , parent  :: Maybe Text
    , time    :: Integer
    } deriving (Generic, Show)

instance ToJSON Comment

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Vandyland.Gallery.Submission(GalleryListing(..), Submission(..), SubmissionListing(..), SubmissionSendable(..)) where

import Data.Aeson(ToJSON)
import Data.UUID(UUID)

import GHC.Generics(Generic)

data SubmissionListing
  = SubmissionListing {
      subName      :: Text
    , isSuppressed :: Bool
    } deriving (Generic, Show, ToJSON)

data GalleryListing
  = GalleryListing {
    galleryName   :: Text
  , isPrescreened :: Bool
  , numWaiting    :: Int
  , numApproved   :: Int
  , creationTime  :: Integer
  , lastSubTime   :: Integer
  } deriving (Generic, Show, ToJSON)

data Submission
  = Submission {
      uploadName'  :: Text
    , base64Image' :: Text
    , token'       :: Maybe UUID
    , modToken'    :: Maybe UUID
    , metadata'    :: Maybe Text
    } deriving Show

data SubmissionSendable
  = SubmissionSendable {
      uploadName   :: Text
    , base64Image  :: Text
    , isOwner      :: Bool
    , canModerate  :: Bool
    , metadata     :: Maybe Text
    } deriving (Generic, Show, ToJSON)

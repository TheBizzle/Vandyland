module Submission(Submission(..)) where

import Bizzlelude

import Data.ByteString(ByteString)

data Submission
  = Submission {
      sessionName :: Text
    , imageBytes  :: ByteString
    , extraData   :: Text
    }

module Submission(Submission(..)) where

import Bizzlelude

data Submission
  = Submission {
      uploadName  :: Text
    , base64Image :: Text
    }

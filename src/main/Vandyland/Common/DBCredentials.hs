{-# LANGUAGE TemplateHaskell #-}
module Vandyland.Common.DBCredentials(password, username) where

import Data.ByteString(ByteString)
import Data.FileEmbed(embedFile)

password :: ByteString
password = $(embedFile ".db_password")

username :: ByteString
username = $(embedFile ".db_username")

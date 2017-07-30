module Main(main) where

import Bizzlelude

import Control.Monad.IO.Class(liftIO)

import Data.Text.Encoding(decodeUtf8)

import Snap.Core(getParam, method, Method(GET, POST), modifyResponse, route, setResponseStatus, Snap, writeText)
import Snap.Http.Server(quickHttpServe)

import NameGen(generateName)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [ ("new-session"                 , method POST handleNewSession)
             , ("uploads/:session-id"         , method GET  handleListSession)
             , ("uploads/:session-id/:item-id", method GET  handleDownloadItem)
             , ("upload"                      , method POST handleUpload)
             ]

handleNewSession :: Snap ()
handleNewSession =
  do
    sessionName <- liftIO generateName
    writeText sessionName

handleListSession :: Snap ()
handleListSession =
  do
    param <- getParam "session-id"
    maybe (notifyBadParams "session ID") (decodeUtf8 >>> writeText) param

handleDownloadItem :: Snap ()
handleDownloadItem =
  do
    sessionID <- getParam "session-id"
    uploadID  <- getParam "item-id"
    let params = sessionID <> uploadID
    maybe (notifyBadParams "session ID or item ID") (decodeUtf8 >>> writeText) params

handleUpload :: Snap ()
handleUpload =
  do
    return ()
    --ps <- getParams
    --writeText $ asText $ show ps
    --upImage <- getPostParam "image"
    --upData  <- getPostParam "data"
    --let params = upImage <> upData
    --maybe (writeBS "must specify echo/param in URL") writeBS params

notifyBadParams :: Text -> Snap ()
notifyBadParams paramDesc =
  do
    modifyResponse $ setResponseStatus 422 "Unprocessable Entity"
    writeText $ "Missing parameter(s): " <> paramDesc

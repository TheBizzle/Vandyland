module Main(main) where

import Bizzlelude

import Control.Monad.IO.Class(liftIO)

import Data.Text.Encoding(decodeUtf8)

import Snap.Core(getParam, getPostParam, method, Method(GET, POST), route, Snap, writeText)
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
    param |> ((fromMaybe "Missing parameter: session ID" ) >>> decodeUtf8 >>> writeText)

handleDownloadItem :: Snap ()
handleDownloadItem =
  do
    param  <- getParam "session-id"
    param2 <- getParam "item-id"
    let params = param <> param2
    params |> ((fromMaybe "Missing parameter: session ID or item ID" ) >>> decodeUtf8 >>> writeText)

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

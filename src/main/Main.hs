module Main(main) where

import Bizzlelude

import Control.Applicative((<|>))
import Control.Lens((#))
import Control.Monad.IO.Class(liftIO)

import Data.Aeson(encode)
import Data.Bifoldable(bimapM_)
import Data.ByteString(ByteString)
import Data.Text.Encoding(decodeUtf8)
import Data.Time(getCurrentTime, utctDay)
import Data.Validation(_Failure, _Success, AccValidation)

import qualified Data.Text.Lazy          as LazyText
import qualified Data.Text.Lazy.Encoding as LazyTextEncoding

import Snap.Core(dir, getParam, method, Method(GET, POST), modifyResponse, route, setResponseStatus, Snap, writeText)
import Snap.Http.Server(quickHttpServe)
import Snap.Util.FileServe(serveDirectory)

import Database(readSubmissionsForSession, retrieveSubmissionData, writeSubmission)
import NameGen(generateName)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [ ("new-session"                 , method POST handleNewSession)
             , ("uploads"                     , method POST handleUpload)
             , ("uploads/:session-id"         , method GET  handleListSession)
             , ("uploads/:session-id/:item-id", method GET  handleDownloadItem)
             ] <|> dir "html" (serveDirectory "html")

handleNewSession :: Snap ()
handleNewSession =
  do
    sessionName <- liftIO generateName
    writeText sessionName

handleListSession :: Snap ()
handleListSession =
  do
    param <- getParamV "session-id"
    bimapM_ notifyBadParams helper param
  where
    helper sessionID =
      do
        submissions <- liftIO $ readSubmissionsForSession sessionID
        writeText $ LazyText.toStrict $ LazyTextEncoding.decodeUtf8 $ encode submissions

handleDownloadItem :: Snap ()
handleDownloadItem =
  do
    sessionID <- getParamV "session-id"
    uploadID  <- getParamV "item-id"
    let params = (,) <$> sessionID <*> uploadID
    bimapM_ notifyBadParams helper params
  where
    helper ps =
      do
        dataMaybe <- liftIO $ (uncurry retrieveSubmissionData) ps
        maybe ((modifyResponse $ setResponseStatus 404 "Not Found") >> (writeText $ "Could not find entry for " <> (asText $ show ps))) writeText dataMaybe

handleUpload :: Snap ()
handleUpload =
  do
    sessionID <- getParamV "session-id"
    upImage   <- getParamV "image"
    upData    <- getParamV "data"
    let tupleV = (,,) <$> sessionID <*> upImage <*> upData
    bimapM_ notifyBadParams submitIt tupleV
  where
    submitIt :: (Text, Text, Text) -> Snap ()
    submitIt (sessionName, image, extraData) =
      do
        millis     <- liftIO getCurrentTime
        let currentTime = utctDay millis
        uploadName <- liftIO generateName
        liftIO $ writeSubmission currentTime uploadName sessionName image extraData
        writeText uploadName

getParamV :: ByteString -> Snap (AccValidation [Text] Text)
getParamV paramName =
  do
    param <- getParam paramName
    return $ maybe (_Failure # [decodeUtf8 paramName]) (\x -> _Success # (decodeUtf8 x)) param

notifyBadParams :: [Text] -> Snap ()
notifyBadParams params =
  do
    modifyResponse $ setResponseStatus 422 "Unprocessable Entity"
    params |> ((fmap ("Missing parameter: " <>)) >>> unlines >>> writeText)

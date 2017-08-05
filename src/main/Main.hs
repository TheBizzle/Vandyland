module Main(main) where

import Bizzlelude

import Control.Applicative((<|>))
import Control.Monad.IO.Class(liftIO)

import Data.Text.Encoding(decodeUtf8)
import Data.Time(getCurrentTime, utctDay)

import Snap.Core(dir, getParam, method, Method(GET, POST), modifyResponse, route, setResponseStatus, Snap, writeText)
import Snap.Http.Server(quickHttpServe)
import Snap.Util.FileServe(serveDirectory)

import Database(retrieveSubmissionData, writeSubmission)
import NameGen(generateName)
import Submission(Submission(Submission))

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
    param <- getParam "session-id"
    maybe (notifyBadParams "session ID") (decodeUtf8 >>> writeText) param

handleDownloadItem :: Snap ()
handleDownloadItem =
  do
    sessionID <- getParam "session-id"
    uploadID  <- getParam "item-id"
    let params = (,) <$> sessionID <*> uploadID
    maybe (notifyBadParams "session ID or item ID") helper params
  where
    helper ps =
      do
        dataMaybe <- ps |> ((decodeUtf8 *** decodeUtf8) >>> (uncurry retrieveSubmissionData) >>> liftIO)
        maybe ((modifyResponse $ setResponseStatus 404 "Not Found") >> (writeText $ "Could not find entry for " <> (asText $ show ps))) writeText dataMaybe

handleUpload :: Snap ()
handleUpload =
  do
    sessionID <- getParam "session-id"
    upImage   <- getParam "image"
    upData    <- getParam "data"
    let submissionMaybe = Submission <$> (fmap decodeUtf8 sessionID) <*> upImage <*> (fmap decodeUtf8 upData)
    maybe (notifyBadParams "image or data or session ID") submitIt submissionMaybe
  where
    submitIt :: Submission -> Snap ()
    submitIt sub =
      do
        millis     <- liftIO getCurrentTime
        let currentTime = utctDay millis
        uploadName <- liftIO generateName
        liftIO $ writeSubmission currentTime uploadName sub
        writeText uploadName

notifyBadParams :: Text -> Snap ()
notifyBadParams paramDesc =
  do
    modifyResponse $ setResponseStatus 422 "Unprocessable Entity"
    writeText $ "Missing parameter(s): " <> paramDesc

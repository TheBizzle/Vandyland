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
import Snap.CORS(applyCORS, defaultOptions)
import Snap.Http.Server(quickHttpServe)
import Snap.Util.FileServe(serveDirectory)

import Database(readSubmissionsForSession, retrieveSubmissionData, writeSubmission)
import NameGen(generateName)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [ ("new-session"                 , allowingCORS POST handleNewSession)
             , ("uploads"                     , allowingCORS POST handleUpload)
             , ("uploads/:session-id"         , allowingCORS GET  handleListSession)
             , ("uploads/:session-id/:item-id", allowingCORS GET  handleDownloadItem)
             ] <|> dir "html" (serveDirectory "html")

handleNewSession :: Snap ()
handleNewSession =
  do
    sessionName <- liftIO generateName
    writeText sessionName

handleListSession :: Snap ()
handleListSession =
  handle1 "session-id" $ \sessionID ->
    do
      submissions <- liftIO $ readSubmissionsForSession sessionID
      writeText $ LazyText.toStrict $ LazyTextEncoding.decodeUtf8 $ encode submissions

handleDownloadItem :: Snap ()
handleDownloadItem =
  handle2 ("session-id", "item-id") $ \ps ->
    do
      dataMaybe <- liftIO $ (uncurry retrieveSubmissionData) ps
      maybe ((modifyResponse $ setResponseStatus 404 "Not Found") >> (writeText $ "Could not find entry for " <> (asText $ show ps))) writeText dataMaybe

handleUpload :: Snap ()
handleUpload =
  handle3 ("session-id", "image", "data") $ \(sessionName, image, extraData) ->
    do
      millis     <- liftIO getCurrentTime
      let currentTime = utctDay millis
      uploadName <- liftIO generateName
      liftIO $ writeSubmission currentTime uploadName sessionName image extraData
      writeText uploadName

handle1 :: ByteString -> (Text -> Snap ()) -> Snap ()
handle1 argName onSuccess =
  do
    arg <- getParamV argName
    bimapM_ notifyBadParams onSuccess arg

handle2 :: (ByteString, ByteString) -> ((Text, Text) -> Snap ()) -> Snap ()
handle2 (arg1Name, arg2Name) onSuccess =
  do
    arg1 <- getParamV arg1Name
    arg2 <- getParamV arg2Name
    let tupleV = (,) <$> arg1 <*> arg2
    bimapM_ notifyBadParams onSuccess tupleV

handle3 :: (ByteString, ByteString, ByteString) -> ((Text, Text, Text) -> Snap ()) -> Snap ()
handle3 (arg1Name, arg2Name, arg3Name) onSuccess =
  do
    arg1 <- getParamV arg1Name
    arg2 <- getParamV arg2Name
    arg3 <- getParamV arg3Name
    let tupleV = (,,) <$> arg1 <*> arg2 <*> arg3
    bimapM_ notifyBadParams onSuccess tupleV

getParamV :: ByteString -> Snap (AccValidation [Text] Text)
getParamV paramName =
  do
    param <- getParam paramName
    return $ maybe (_Failure # [decodeUtf8 paramName]) (\x -> _Success # (decodeUtf8 x)) param

allowingCORS :: Method -> Snap () -> Snap ()
allowingCORS mthd f = applyCORS defaultOptions $ method mthd f

notifyBadParams :: [Text] -> Snap ()
notifyBadParams params =
  do
    modifyResponse $ setResponseStatus 422 "Unprocessable Entity"
    params |> ((fmap ("Missing parameter: " <>)) >>> unlines >>> writeText)

module Main(main) where

import Bizzlelude

import Control.Applicative((<|>))
import Control.Lens((#))
import Control.Monad.IO.Class(liftIO)

import Data.Aeson(encode, ToJSON)
import Data.Bifoldable(bimapM_)
import Data.ByteString(ByteString)
import Data.Text.Encoding(decodeUtf8)
import Data.Time(getCurrentTime, utctDay)
import Data.Validation(_Failure, _Success, AccValidation)

import qualified Data.Text.Lazy          as LazyText
import qualified Data.Text.Lazy.Encoding as LazyTextEncoding

import Snap.Core(dir, getParam, method, Method(GET, POST), modifyResponse, route, setContentType, setResponseStatus, Snap, writeText)
import Snap.CORS(applyCORS, defaultOptions)
import Snap.Http.Server(quickHttpServe)
import Snap.Util.FileServe(serveDirectory)

import Database(readSubmissionsForSession, retrieveSubmissionData, writeSubmission)
import NameGen(generateName)

data Constraint
  = NonEmpty

type Arg = (ByteString, [Constraint])

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [ ("new-session"                 , allowingCORS POST handleNewSession)
             , ("uploads"                     , allowingCORS POST handleUpload)
             , ("uploads/:session-id"         , allowingCORS GET  handleListSession)
             , ("uploads/:session-id/:item-id", allowingCORS GET  handleDownloadItem)
             ] <|> dir "html" (serveDirectory "html")

handleNewSession :: Snap ()
handleNewSession = generateName |> (liftIO >=> writeText)

handleListSession :: Snap ()
handleListSession = handle1 ("session-id", [NonEmpty]) $ readSubmissionsForSession >>> liftIO >=> encodeText >>> (succeed "application/json")

handleDownloadItem :: Snap ()
handleDownloadItem =
  handle2 (("session-id", [NonEmpty]), ("item-id", [NonEmpty])) $ \ps ->
    do
      dataMaybe <- liftIO $ (uncurry retrieveSubmissionData) ps
      maybe (failWith 404 (writeText $ "Could not find entry for " <> (asText $ show ps))) (succeed "text/plain") dataMaybe

handleUpload :: Snap ()
handleUpload =
  handle3 (("session-id", [NonEmpty]), ("image", []), ("data", [])) $ \(sessionName, image, extraData) ->
    do
      millis     <- liftIO getCurrentTime
      let currentTime = utctDay millis
      uploadName <- liftIO generateName
      liftIO $ writeSubmission currentTime uploadName sessionName image extraData
      writeText uploadName

handle1 :: Arg -> (Text -> Snap ()) -> Snap ()
handle1 arg onSuccess =
  do
    arg <- getParamV arg
    bimapM_ notifyBadParams onSuccess arg

handle2 :: (Arg, Arg) -> ((Text, Text) -> Snap ()) -> Snap ()
handle2 (arg1, arg2) onSuccess =
  do
    arg1 <- getParamV arg1
    arg2 <- getParamV arg2
    let tupleV = (,) <$> arg1 <*> arg2
    bimapM_ notifyBadParams onSuccess tupleV

handle3 :: (Arg, Arg, Arg) -> ((Text, Text, Text) -> Snap ()) -> Snap ()
handle3 (arg1, arg2, arg3) onSuccess =
  do
    arg1 <- getParamV arg1
    arg2 <- getParamV arg2
    arg3 <- getParamV arg3
    let tupleV = (,,) <$> arg1 <*> arg2 <*> arg3
    bimapM_ notifyBadParams onSuccess tupleV

encodeText :: ToJSON a => a -> Text
encodeText = encode >>> LazyTextEncoding.decodeUtf8 >>> LazyText.toStrict

getParamV :: Arg -> Snap (AccValidation [Text] Text)
getParamV (paramName, constraints) =
  do
    param <- getParam paramName
    let deconstrained = deconstrain constraints param
    return $ maybe (_Failure # [decodeUtf8 paramName]) (\x -> _Success # (decodeUtf8 x)) deconstrained
  where
    deconstrain _              Nothing = Nothing
    deconstrain []                   x = x
    deconstrain (NonEmpty:_) (Just "") = Nothing
    deconstrain (NonEmpty:t)         x = deconstrain t x

allowingCORS :: Method -> Snap () -> Snap ()
allowingCORS mthd f = applyCORS defaultOptions $ method mthd f

notifyBadParams :: [Text] -> Snap ()
notifyBadParams = (fmap ("Missing parameter: " <>)) >>> unlines >>> writeText >>> (failWith 422)

failWith :: Int -> Snap () -> Snap ()
failWith x snap =
  do
    modifyResponse $ setResponseStatus x $ statusName x
    snap
  where
    statusName 404 = "Not Found"
    statusName 422 = "Unprocessable Entity"
    statusName _   = error "Unhandled status"

succeed :: ByteString -> Text -> Snap ()
succeed contentType output =
  do
    modifyResponse $ setContentType contentType
    writeText output

module Main(main) where

import Bizzlelude

import Control.Applicative((<|>))
import Control.Lens((#))
import Control.Monad.IO.Class(liftIO)

import Data.Bifoldable(bimapM_)
import Data.Validation(_Failure, _Success, AccValidation)

import qualified Data.Map           as Map
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.UUID          as UUID

import Snap.Core(dir, getParam, Method(GET, POST), route, Snap, writeText)
import Snap.Http.Server(quickHttpServe)
import Snap.Util.FileServe(serveDirectory)
import Snap.Util.GZip(withCompression)

import Database(readCommentsFor, readSubmissionData, readSubmissionsLite, readSubmissionNames, writeComment, writeSubmission)
import NameGen(generateName)
import SnapHelpers(allowingCORS, Constraint(NonEmpty), decodeText, encodeText, failWith, getParamV, handle1, handle2, handle5, notifyBadParams, succeed, uncurry4, withFileUploads)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [ ("echo/:param"                          ,                   allowingCORS POST handleEchoData)
             , ("new-session"                          ,                   allowingCORS POST handleNewSession)
             , ("uploads"                              ,                   allowingCORS POST handleUpload)
             , ("file-uploads"                         ,                   allowingCORS POST handleUploadFile)
             , ("uploads/:session-id/:item-id"         , withCompression $ allowingCORS GET  handleDownloadItem)
             , ("comments"                             ,                   allowingCORS POST handleSubmitComment)
             , ("comments/:session-id/:item-id"        , withCompression $ allowingCORS GET  handleGetComments)
             , ("names/:session-id"                    , withCompression $ allowingCORS GET  handleListSession)
             , ("data-lite"                            , withCompression $ allowingCORS POST handleSubmissionsLite)
             ] <|> dir "html" (serveDirectory "html")

handleEchoData :: Snap ()
handleEchoData = handle1 ("param", [NonEmpty]) $ \param -> withFileUploads $ \fileMap -> do
  prm <- getParam $ TextEncoding.encodeUtf8 param
  maybe (notifyBadParams [param]) writeText ((map TextEncoding.decodeUtf8 prm) <|> (Map.lookup param fileMap))

handleNewSession :: Snap ()
handleNewSession = generateName |> (liftIO >=> writeText)

handleListSession :: Snap ()
handleListSession = handle1 ("session-id", [NonEmpty]) $ readSubmissionNames >>> liftIO >=> encodeText >>> (succeed "application/json")

handleDownloadItem :: Snap ()
handleDownloadItem =
  handle2 (("session-id", [NonEmpty]), ("item-id", [NonEmpty])) $ \ps ->
    do
      dataMaybe <- liftIO $ (uncurry readSubmissionData) ps
      maybe (failWith 404 (writeText $ "Could not find entry for " <> (asText $ show ps))) (succeed "text/plain") dataMaybe

handleSubmissionsLite :: Snap ()
handleSubmissionsLite =
  handle2 (("session-id", [NonEmpty]), ("names", [])) $ \(sessionID, namesText) ->
    do
      let names = decodeText namesText :: Maybe [Text]
      maybe (failWith 422 (writeText $ "Parameter 'names' is invalid JSON: " <> namesText)) ((readSubmissionsLite sessionID) >>> liftIO >=> encodeText >>> (succeed "application/json")) names

handleUpload :: Snap ()
handleUpload = (getParamV ("data", [])) >>= handleUploadHelper

handleUploadFile :: Snap ()
handleUploadFile = withFileUploads $ (lookupParam "data") >>> handleUploadHelper
  where
    lookupParam param fileMap = maybe (_Failure # [param]) (_Success #) $ Map.lookup param fileMap

handleUploadHelper :: AccValidation [Text] Text -> Snap ()
handleUploadHelper datum =
  do
    sessionID <- getParamV ("session-id", [NonEmpty])
    image     <- getParamV ("image"     , [])
    metadata  <- getParamV ("metadata"  , [NonEmpty])
    let tupleV = (,,,) <$> sessionID <*> image <*> (map Just metadata <> (_Success # Nothing)) <*> datum
    bimapM_ notifyBadParams ((uncurry4 writeSubmission) >>> liftIO >=> writeText) tupleV

handleGetComments :: Snap ()
handleGetComments = handle2 (("session-id", [NonEmpty]), ("item-id", [NonEmpty])) $ (uncurry readCommentsFor) >>> liftIO >=> encodeText >>> (succeed "application/json")

handleSubmitComment :: Snap ()
handleSubmitComment =
  handle5 (("session-id", [NonEmpty]), ("item-id", [NonEmpty]), ("comment", [NonEmpty]), ("author", [NonEmpty]), ("parent", [])) $
    \(sessionName, uploadName, comment, author, parent) ->
      do
        liftIO $ writeComment comment uploadName sessionName author (UUID.fromText parent)
        writeText "" -- Necessary?

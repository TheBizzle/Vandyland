module Vandyland.Gallery.Controller(routes) where

import Control.Applicative((<|>))
import Control.Lens((#))
import Control.Monad.IO.Class(liftIO)

import Data.Bifoldable(bimapM_)
import Data.ByteString(ByteString)
import Data.Validation(_Failure, _Success, Validation)

import qualified Data.Map           as Map
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.UUID          as UUID

import Snap.Core(getParam, Method(GET, POST), Snap, writeText)
import Snap.Util.GZip(withCompression)

import Vandyland.Common.SnapHelpers(allowingCORS, Arg(Arg), decodeText, encodeText, failWith, free, getParamV, handle1, handle2, handle5, nonEmpty, notifyBadParams, succeed, withFileUploads)

import Vandyland.Gallery.Database(readCommentsFor, readSubmissionData, readSubmissionsLite, readSubmissionNames, uniqueSessionName, writeComment, writeSubmission)

routes :: [(ByteString, Snap ())]
routes = [ ("echo/:param"                          ,                   allowingCORS POST handleEchoData)
         , ("new-session"                          ,                   allowingCORS POST handleNewSession)
         , ("uploads"                              ,                   allowingCORS POST handleUpload)
         , ("file-uploads"                         ,                   allowingCORS POST handleUploadFile)
         , ("uploads/:session-id/:item-id"         , withCompression $ allowingCORS GET  handleDownloadItem)
         , ("comments"                             ,                   allowingCORS POST handleSubmitComment)
         , ("comments/:session-id/:item-id"        , withCompression $ allowingCORS GET  handleGetComments)
         , ("names/:session-id"                    , withCompression $ allowingCORS GET  handleListSession)
         , ("data-lite"                            , withCompression $ allowingCORS POST handleSubmissionsLite)
         ]

handleEchoData :: Snap ()
handleEchoData = handle1 (Arg "param" nonEmpty) $ \param -> withFileUploads $ \fileMap -> do
  prm <- getParam $ TextEncoding.encodeUtf8 param
  maybe (notifyBadParams [param]) writeText ((map TextEncoding.decodeUtf8 prm) <|> (Map.lookup param fileMap))

handleNewSession :: Snap ()
handleNewSession = uniqueSessionName |> (liftIO >=> writeText)

handleListSession :: Snap ()
handleListSession = handle1 (Arg "session-id" nonEmpty) $ readSubmissionNames >>> liftIO >=> encodeText >>> (succeed "application/json")

handleDownloadItem :: Snap ()
handleDownloadItem =
  handle2 (Arg "session-id" nonEmpty, Arg "item-id" nonEmpty) $ \ps ->
    do
      dataMaybe <- liftIO $ (uncurry readSubmissionData) ps
      maybe (failWith 404 (writeText $ "Could not find entry for " <> (asText $ show ps))) (succeed "text/plain") dataMaybe

handleSubmissionsLite :: Snap ()
handleSubmissionsLite =
  handle2 (Arg "session-id" nonEmpty, Arg "names" free) $ \(sessionID, namesText) ->
    do
      let names = decodeText namesText :: Maybe [Text]
      maybe (failWith 422 (writeText $ "Parameter 'names' is invalid JSON: " <> namesText)) ((readSubmissionsLite sessionID) >>> liftIO >=> encodeText >>> (succeed "application/json")) names

handleUpload :: Snap ()
handleUpload =
  do
    dataV  <- getParamV $ Arg "data"  free
    imageV <- getParamV $ Arg "image" free
    handleUploadHelper dataV imageV

handleUploadFile :: Snap ()
handleUploadFile = withFileUploads $ \fileMap -> handleUploadHelper (lookupParam "data" fileMap) (lookupParam "image" fileMap)
  where
    lookupParam param fileMap = maybe (_Failure # [param]) (_Success #) $ Map.lookup param fileMap

handleUploadHelper :: Validation [Text] Text -> Validation [Text] Text -> Snap ()
handleUploadHelper datum image =
  do
    sessionID <- getParamV $ Arg "session-id" nonEmpty
    metadata  <- getParamV $ Arg "metadata"   nonEmpty
    let tupleV = (,,,) <$> sessionID <*> image <*> (map Just metadata <> (_Success # Nothing)) <*> datum
    bimapM_ notifyBadParams ((uncurry4 writeSubmission) >>> liftIO >=> writeText) tupleV

handleGetComments :: Snap ()
handleGetComments = handle2 (Arg "session-id" nonEmpty, Arg "item-id" nonEmpty) $ (uncurry readCommentsFor) >>> liftIO >=> encodeText >>> (succeed "application/json")

handleSubmitComment :: Snap ()
handleSubmitComment =
  handle5 (Arg "session-id" nonEmpty, Arg "item-id" nonEmpty, Arg "comment" nonEmpty, Arg "author" nonEmpty, Arg "parent" free) $
    \(sessionName, uploadName, comment, author, parent) ->
      do
        liftIO $ writeComment comment uploadName sessionName author (UUID.fromText parent)
        writeText "" -- Necessary?
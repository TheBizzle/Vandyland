module Vandyland.Gallery.Controller(routes) where

import Control.Lens((#))

import Data.Bifoldable(bimapM_)
import Data.ByteString(ByteString)
import Data.Validation(_Failure, _Success, Validation)

import qualified Data.Map           as Map
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.UUID          as UUID
import qualified Data.UUID.V4       as UUIDGen

import Snap.Core(getParam, Method(DELETE, GET, POST), Snap, writeText)
import Snap.Util.GZip(withCompression)

import Vandyland.Common.SnapHelpers(allowingCORS, Arg(Arg), decodeText, encodeText, failWith, free, getParamV, getParamVM, handle1, handle2, handle3, handle5, nonEmpty, notifyBadParams, succeed, withFileUploads)

import Vandyland.Gallery.Database(SuppressionResult(Suppressed, NotAuthorized, NotFound), readCommentsFor, readSubmissionData, readSubmissionsLite, readSubmissionListings, suppressSubmission, uniqueSessionName, writeComment, writeSubmission)
import Vandyland.Gallery.Submission(Submission(Submission), SubmissionSendable(SubmissionSendable))

routes :: [(ByteString, Snap ())]
routes = [ ("echo/:param"                          ,                   allowingCORS POST   handleEchoData)
         , ("new-session"                          ,                   allowingCORS POST   handleNewSession)
         , ("uploads"                              ,                   allowingCORS POST   handleUpload)
         , ("file-uploads"                         ,                   allowingCORS POST   handleUploadFile)
         , ("uploads/:session-id/:item-id"         , withCompression $ allowingCORS GET    handleDownloadItem)
         , ("uploads/:session-id/:item-id/:token"  , withCompression $ allowingCORS DELETE handleSuppressItem)
         , ("comments"                             ,                   allowingCORS POST   handleSubmitComment)
         , ("comments/:session-id/:item-id"        , withCompression $ allowingCORS GET    handleGetComments)
         , ("listings/:session-id"                 , withCompression $ allowingCORS GET    handleListSession)
         , ("data-lite"                            , withCompression $ allowingCORS POST   handleSubmissionsLite)
         , ("uploader-token"                       ,                   allowingCORS GET    handleGetUploaderToken)
         ]

handleEchoData :: Snap ()
handleEchoData = handle1 (Arg "param" nonEmpty) $ \param -> withFileUploads $ \fileMap -> do
  prm <- getParam $ TextEncoding.encodeUtf8 param
  maybe (notifyBadParams [param]) writeText ((map TextEncoding.decodeUtf8 prm) <|> (Map.lookup param fileMap))

handleNewSession :: Snap ()
handleNewSession = uniqueSessionName |> liftIO &>= writeText

handleListSession :: Snap ()
handleListSession = handle1 (Arg "session-id" nonEmpty) $ readSubmissionListings &> liftIO &>= (encodeText &> (succeed "application/json"))

handleDownloadItem :: Snap ()
handleDownloadItem =
  handle2 (Arg "session-id" nonEmpty, Arg "item-id" nonEmpty) $ \ps ->
    do
      dataMaybe <- liftIO $ (uncurry readSubmissionData) ps
      maybe (failWith 404 (writeText $ "Could not find entry for " <> (asText $ show ps))) (succeed "text/plain") dataMaybe

handleSuppressItem :: Snap ()
handleSuppressItem =
  handle3 (Arg "session-id" nonEmpty, Arg "item-id" nonEmpty, Arg "token" nonEmpty) $ \(sid, iid, token) ->
    do
      result <- liftIO $ (suppressSubmission sid iid $ UUID.fromText token)
      case result of
        Suppressed    ->                writeText "Submission successfully suppressed"
        NotAuthorized -> failWith 401 $ writeText "You are not authorized to modify that submission"
        NotFound      -> failWith 404 $ writeText "Could not find a matching submission to suppress"

handleGetUploaderToken :: Snap ()
handleGetUploaderToken = (UUIDGen.nextRandom <&> UUID.toText) |> liftIO &>= (succeed "text/plain")

handleSubmissionsLite :: Snap ()
handleSubmissionsLite =
  handle3 (Arg "session-id" nonEmpty, Arg "names" free, Arg "token" free) $ \(sessionID, namesText, token) ->
    do
      let names = decodeText namesText :: Maybe [Text]
      maybe (failWith 422 (writeText $ "Parameter 'names' is invalid JSON: " <> namesText))
            ((readSubmissionsLite sessionID) &> (map $ map $ checkOwnership $ UUID.fromText token) &> liftIO &>= (encodeText &> (succeed "application/json"))) names
  where
    checkOwnership _ (Submission name b64 _ meta) = SubmissionSendable name b64 meta

handleUpload :: Snap ()
handleUpload =
  do
    dataV  <- getParamV $ Arg "data"  free
    imageV <- getParamV $ Arg "image" free
    handleUploadHelper dataV imageV Map.empty

handleUploadFile :: Snap ()
handleUploadFile = withFileUploads $ \fileMap -> handleUploadHelper (lookupParam "data" fileMap) (lookupParam "image" fileMap) fileMap
  where
    lookupParam param fileMap = maybe (_Failure # [param]) (_Success #) $ Map.lookup param fileMap

handleUploadHelper :: Validation [Text] Text -> Validation [Text] Text -> Map Text Text -> Snap ()
handleUploadHelper datum image fileMap =
  do
    sessionID <- getParamVM fileMap $ Arg "session-id" nonEmpty
    metadata  <- getParamVM fileMap $ Arg "metadata"   nonEmpty
    token     <- getParamVM fileMap $ Arg "token"      nonEmpty
    let tupleV = (,,,,) <$> sessionID <*> image <*> (defaultOnV token) <*> (defaultOnV metadata) <*> datum
    bimapM_ notifyBadParams ((uncurry5 writeSubmission) &> liftIO &>= writeText) tupleV
  where
    defaultOnV v = (map Just v) <> (_Success # Nothing)

handleGetComments :: Snap ()
handleGetComments = handle2 (Arg "session-id" nonEmpty, Arg "item-id" nonEmpty) $ (uncurry readCommentsFor) &> liftIO &>= (encodeText &> (succeed "application/json"))

handleSubmitComment :: Snap ()
handleSubmitComment =
  handle5 (Arg "session-id" nonEmpty, Arg "item-id" nonEmpty, Arg "comment" nonEmpty, Arg "author" nonEmpty, Arg "parent" free) $
    \(sessionName, uploadName, comment, author, parent) ->
      do
        liftIO $ writeComment comment uploadName sessionName author (UUID.fromText parent)
        writeText "" -- Necessary?

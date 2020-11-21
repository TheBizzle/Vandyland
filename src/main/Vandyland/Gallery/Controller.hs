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

import Vandyland.Common.SnapHelpers(allowingCORS, Arg(Arg), asBool, asUUID, decodeText, encodeText, failWith, free, getParamV, getParamVM, handle1, handle2, handle3, handle5, nonEmpty, notifyBadParams, succeed, withFileUploads)

import Vandyland.Gallery.Database(approveSubmission, forbidSubmission, PrivilegedActionResult(Fulfilled, NotAuthorized, NotFound), readCommentsFor, readGalleryListings, readSubmissionData, readSubmissionsLite, readSubmissionListings, readSubmissionListingsForModeration, registerNewSession, suppressSubmission, uniqueSessionName, writeComment, writeSubmission)
import Vandyland.Gallery.Submission(Submission(Submission), SubmissionSendable(SubmissionSendable))

routes :: [(ByteString, Snap ())]
routes = [ ("echo/:param"                                     ,      ac POST   handleEchoData)
         , ("new-session"                                     ,      ac POST   handleNewSession)
         , ("new-session/:session-id/:gets-prescreened/:token",      ac POST   handleNewSessionWithParams)
         , ("uploads"                                         ,      ac POST   handleUpload)
         , ("file-uploads"                                    ,      ac POST   handleUploadFile)
         , ("uploads/:session-id/:item-id"                    , wc $ ac GET    handleDownloadItem)
         , ("uploads/:session-id/:item-id/:token"             , wc $ ac GET    handleDownloadItemWithToken)
         , ("uploads/:session-id/:item-id/:token"             ,      ac DELETE handleSuppressItem)
         , ("uploads/:session-id/:item-id/:token/approve"     ,      ac POST   handleApproveItem)
         , ("uploads/:session-id/:item-id/:token/reject"      ,      ac POST   handleForbidItem)
         , ("comments"                                        ,      ac POST   handleSubmitComment)
         , ("comments/:session-id/:item-id"                   , wc $ ac GET    handleGetComments)
         , ("gallery-listings/:token"                         , wc $ ac GET    handleListGalleries)
         , ("listings/:session-id"                            , wc $ ac GET    handleListSession)
         , ("mod-listings/:session-id/:token"                 , wc $ ac GET    handleListSessionForModeration)
         , ("data-lite"                                       , wc $ ac POST   handleSubmissionsLite)
         , ("data-lite/:token"                                , wc $ ac POST   handleSubmissionsLiteWithToken)
         , ("moderator-token"                                 ,      ac GET    handleGetModeratorToken)
         , ("uploader-token"                                  ,      ac GET    handleGetUploaderToken)
         ]
  where
    wc = withCompression
    ac = allowingCORS

handleEchoData :: Snap ()
handleEchoData = handle1 (Arg "param" nonEmpty) $ \param -> withFileUploads $ \fileMap -> do
  prm <- getParam $ TextEncoding.encodeUtf8 param
  maybe (notifyBadParams [param]) writeText ((map TextEncoding.decodeUtf8 prm) <|> (Map.lookup param fileMap))

handleNewSession :: Snap ()
handleNewSession =
  do
    name <- liftIO $ uniqueSessionName
    _handleNewSessionWithParams name False Nothing

handleNewSessionWithParams :: Snap ()
handleNewSessionWithParams =
  handle3 (Arg "session-id" nonEmpty, Arg "gets-prescreened" asBool, Arg "token" asUUID) $ \(sid, gps, token) ->
    _handleNewSessionWithParams sid gps $ Just token

handleListGalleries :: Snap ()
handleListGalleries = handle1 (Arg "token" asUUID) $
  readGalleryListings &> liftIO &>= (encodeText &> (succeed "application/json"))

handleListSession :: Snap ()
handleListSession = handle1 (Arg "session-id" nonEmpty) $
  readSubmissionListings &> liftIO &>= (encodeText &> (succeed "application/json"))

handleListSessionForModeration :: Snap ()
handleListSessionForModeration = handle2 (Arg "session-id" nonEmpty, Arg "token" asUUID) $ \(sid, token) ->
  do
    result <- liftIO $ readSubmissionListingsForModeration sid token
    case result of
      Fulfilled xs  -> xs |> encodeText &> succeed "application/json"
      NotAuthorized -> failWith 401 $ writeText "You are not authorized to read those submissions"
      NotFound      -> failWith 401 $ writeText "You are not authorized to read those submissions"
      -- I think it would be a security mistake to let any authorized party know when there are things here for reading.
      -- ~~JAB (11/1/20)

handleDownloadItem :: Snap ()
handleDownloadItem =
  handle2 (Arg "session-id" nonEmpty, Arg "item-id" nonEmpty) $ \ps@(sid, iid) ->
    do
      dataResult <- liftIO $ readSubmissionData sid iid Nothing
      case dataResult of
        Fulfilled dta -> succeed "text/plain" dta
        NotAuthorized -> failWith 401 $ writeText $ "You are not authorized to download that."
        NotFound      -> failWith 404 $ writeText $ "Could not find entry for " <> (asText $ show ps)

handleDownloadItemWithToken :: Snap ()
handleDownloadItemWithToken =
  handle3 (Arg "session-id" nonEmpty, Arg "item-id" nonEmpty, Arg "token" asUUID) $ \ps@(sid, iid, token) ->
    do
      dataResult <- liftIO $ readSubmissionData sid iid $ Just token
      case dataResult of
        Fulfilled dta -> succeed "text/plain" dta
        NotAuthorized -> failWith 401 $ writeText $ "You are not authorized to download that."
        NotFound      -> failWith 404 $ writeText $ "Could not find entry for " <> (asText $ show ps)

handleSuppressItem :: Snap ()
handleSuppressItem =
  handle3 (Arg "session-id" nonEmpty, Arg "item-id" nonEmpty, Arg "token" asUUID) $ \(sid, iid, token) ->
    do
      result <- liftIO $ suppressSubmission sid iid token
      case result of
        Fulfilled _   ->                writeText "Submission successfully suppressed"
        NotAuthorized -> failWith 401 $ writeText "You are not authorized to modify that submission"
        NotFound      -> failWith 404 $ writeText "Could not find a matching submission to suppress"

handleApproveItem :: Snap ()
handleApproveItem =
  handle3 (Arg "session-id" nonEmpty, Arg "item-id" nonEmpty, Arg "token" asUUID) $ \(sid, iid, token) ->
    do
      result <- liftIO $ approveSubmission sid iid $ token
      case result of
        Fulfilled _   ->                writeText "Submission approved"
        NotAuthorized -> failWith 401 $ writeText "You are not authorized to modify that submission"
        NotFound      -> failWith 404 $ writeText "Could not find a matching submission to approve"

handleForbidItem :: Snap ()
handleForbidItem =
  handle3 (Arg "session-id" nonEmpty, Arg "item-id" nonEmpty, Arg "token" asUUID) $ \(sid, iid, token) ->
    do
      result <- liftIO $ forbidSubmission sid iid $ token
      case result of
        Fulfilled _   ->                writeText "Submission successfully forbidden"
        NotAuthorized -> failWith 401 $ writeText "You are not authorized to modify that submission"
        NotFound      -> failWith 404 $ writeText "Could not find a matching submission to forbid"

handleGetModeratorToken :: Snap ()
handleGetModeratorToken = genToken |> liftIO &>= (succeed "text/plain")

handleGetUploaderToken :: Snap ()
handleGetUploaderToken = genToken |> liftIO &>= (succeed "text/plain")

genToken :: IO Text
genToken = UUIDGen.nextRandom <&> UUID.toText

handleSubmissionsLite :: Snap ()
handleSubmissionsLite =
  handle2 (Arg "session-id" nonEmpty, Arg "names" free) $ \(sessionID, namesText) ->
    do
      let names = decodeText namesText :: Maybe [Text]
      maybe (failWith 422 (writeText $ "Parameter 'names' is invalid JSON: " <> namesText))
            ((readSubmissionsLite sessionID Nothing) &> (map $ map $ convert) &> liftIO &>= (encodeText &> (succeed "application/json"))) names
  where
    convert (Submission name b64 _ _ meta) =
      SubmissionSendable name b64 False False meta

handleSubmissionsLiteWithToken :: Snap ()
handleSubmissionsLiteWithToken =
  handle3 (Arg "session-id" nonEmpty, Arg "names" free, Arg "token" asUUID) $ \(sessionID, namesText, token) ->
    do
      let names = decodeText namesText :: Maybe [Text]
      maybe (failWith 422 (writeText $ "Parameter 'names' is invalid JSON: " <> namesText))
            ((readSubmissionsLite sessionID $ Just token) &> (map $ map $ checkOwnership $ Just token) &> liftIO &>= (encodeText &> (succeed "application/json"))) names
  where
    validates (Just a) (Just b) = a == b
    validates _        _        = False
    checkOwnership token (Submission name b64 stoken mtoken meta) =
      SubmissionSendable name b64 (stoken `validates` token) (mtoken `validates` token) meta

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
    token     <- getParamVM fileMap $ Arg "token"      asUUID
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

_handleNewSessionWithParams :: Text -> Bool -> Maybe UUID.UUID -> Snap ()
_handleNewSessionWithParams name getsPrescreened token =
  do
    wasSuccessful <- liftIO $ registerNewSession name getsPrescreened token
    if wasSuccessful then
      writeText name
    else
      failWith 409 $ writeText $ "A session with the name '" <> name <> "' already exists."

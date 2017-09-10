module Main(main) where

import Bizzlelude

import Control.Applicative((<|>))
import Control.Lens((#))
import Control.Monad.IO.Class(liftIO)

import Data.Bifoldable(bimapM_)
import Data.Text.Encoding(decodeUtf8)
import Data.Validation(_Success)

import qualified Data.Map  as Map
import qualified Data.UUID as UUID

import Snap.Core(dir, getParam, Method(GET, POST), route, Snap, writeText)
import Snap.Http.Server(quickHttpServe)
import Snap.Util.FileServe(serveDirectory)
import Snap.Util.GZip(withCompression)

import Database(readCommentsFor, readSubmissionsForSession, retrieveSubmissionData, writeComment, writeSubmission)
import NameGen(generateName)
import SnapHelpers(allowingCORS, Constraint(NonEmpty), encodeText, failWith, getParamV, handle1, handle2, handle5, handleUploadsTo, notifyBadParams, succeed)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [ ("new-session"                          ,                   allowingCORS POST handleNewSession)
             , ("uploads"                              ,                   allowingCORS POST handleUpload)
             , ("comments"                             ,                   allowingCORS POST handleSubmitComment)
             , ("echo"                                 ,                   allowingCORS POST handleEchoData)
             , ("uploads/:session-id"                  , withCompression $ allowingCORS GET  handleListSession)
             , ("uploads/:session-id/:item-id"         , withCompression $ allowingCORS GET  handleDownloadItem)
             , ("uploads/:session-id/:item-id/comments", withCompression $ allowingCORS GET  handleGetComments)
             ] <|> dir "html" (serveDirectory "html")

handleEchoData :: Snap ()
handleEchoData = (handleUploadsTo "dist/filetmp") >>= (bimapM_ fail succeed)
  where
    fail             = unlines >>> writeText >>> failWith 400
    succeed          = lookupFold (\k -> notifyBadParams [k]) writeText "data"
    lookupFold f g k = (Map.lookup k) >>> (maybe (f k) g)

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
      uploadName <- liftIO $ writeSubmission sessionName image extraData
      writeText uploadName

handleGetComments :: Snap ()
handleGetComments = handle2 (("session-id", [NonEmpty]), ("item-id", [NonEmpty])) $ (uncurry readCommentsFor) >>> liftIO >=> encodeText >>> (succeed "application/json")

handleSubmitComment :: Snap ()
handleSubmitComment =
  handle5 (("session-id", [NonEmpty]), ("item-id", [NonEmpty]), ("comment", [NonEmpty]), ("author", [NonEmpty]), ("parent", [])) $
    \(sessionName, uploadName, comment, author, parent) ->
      do
        liftIO $ writeComment comment uploadName sessionName author (UUID.fromText parent)
        writeText "" -- Necessary?

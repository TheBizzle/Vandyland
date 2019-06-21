module Vandyland.BadgerState.Controller(routes) where

import Data.ByteString(ByteString)
import Data.Time(UTCTime)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

import qualified Data.UUID as UUID

import Snap.Core(Method(GET, POST), Snap, writeText)
import Snap.Util.GZip(withCompression)

import Vandyland.Common.SnapHelpers(allowingCORS, Arg(Arg), asNonNegInt, asUUID, encodeText, failWith, handle1, handle2, handle3, nonEmpty, succeed)

import Vandyland.BadgerState.Database(joinGroup, readDataFor, readGroup, readNDataFor, readSignalFor, writeData, writeSignal)
import Vandyland.BadgerState.Datum(Datum(Datum))

routes :: [(ByteString, Snap ())]
routes = [ ("badgerstate/join/:group-id"                     , withCompression $ allowingCORS POST handleJoinGroup  )
         , ("badgerstate/participants/:group-id"             , withCompression $ allowingCORS GET  handleListGroup  )
         , ("badgerstate/data/:group-id/:bucket-id/:data"    , withCompression $ allowingCORS POST handlePostData   )
         , ("badgerstate/data/:group-id/:bucket-id/:ts"      , withCompression $ allowingCORS GET  handleFetchData  )
         , ("badgerstate/n-data/:group-id/:bucket-id/:n"     , withCompression $ allowingCORS GET  handleFetchNData )
         , ("badgerstate/signal/:group-id/:bucket-id/:signal", withCompression $ allowingCORS POST handlePostSignal )
         , ("badgerstate/signal/:group-id/:bucket-id"        , withCompression $ allowingCORS GET  handleFetchSignal)
         ]

handleJoinGroup :: Snap ()
handleJoinGroup =
  handle1 (Arg "group-id" nonEmpty) $
    joinGroup &> liftIO &>= (UUID.toText &> (succeed "text/plain"))

handleListGroup :: Snap ()
handleListGroup =
  handle1 (Arg "group-id" nonEmpty) $
    readGroup &> liftIO &>= (encodeText &> (succeed "application/json"))

handlePostData :: Snap ()
handlePostData =
  handle3 (Arg "group-id" nonEmpty, Arg "bucket-id" asUUID, Arg "data" nonEmpty) $
    (uncurry3 writeData) &> liftIO &>= (timeToText &> (succeed "text/plain"))

handleFetchData :: Snap ()
handleFetchData =
  handle3 (Arg "group-id" nonEmpty, Arg "bucket-id" asUUID, Arg "ts" asNonNegInt) $
    (\(g, b, t) -> readDataFor g b $ intToTime t) &> liftIO &>= ((map mkDatum) &> encodeText &> (succeed "application/json"))

handleFetchNData :: Snap ()
handleFetchNData =
  handle3 (Arg "group-id" nonEmpty, Arg "bucket-id" asUUID, Arg "n" asNonNegInt) $
    (uncurry3 readNDataFor) &> liftIO &>= ((map mkDatum) &> encodeText &> (succeed "application/json"))

handlePostSignal :: Snap ()
handlePostSignal =
  handle3 (Arg "group-id" nonEmpty, Arg "bucket-id" asUUID, Arg "signal" nonEmpty) $
    (uncurry3 writeSignal) &> liftIO &>= (timeToText &> (succeed "text/plain"))

handleFetchSignal :: Snap ()
handleFetchSignal =
  handle2 (Arg "group-id" nonEmpty, Arg "bucket-id" asUUID) $ (\(groupID, bucketID) ->
    (readSignalFor groupID bucketID) |> (liftIO &>=
      maybe (failWith 404 (writeText $ "No signal for " <> groupID <> "/" <> (UUID.toText bucketID) <> ""))
            (\(signal, timestamp) -> succeed "text/plain" ((timeToText timestamp) <> " | " <> signal)))
  )

mkDatum :: (Text, UTCTime) -> Datum
mkDatum (value, time) = Datum value $ timeToInteger time

timeToText :: UTCTime -> Text
timeToText = timeToInteger &> showText

timeToInteger :: UTCTime -> Integer
timeToInteger = utcTimeToPOSIXSeconds &> (* 1000000) &> floor

intToTime :: Int -> UTCTime
intToTime = fromIntegral &> (/ 1000000) &> posixSecondsToUTCTime

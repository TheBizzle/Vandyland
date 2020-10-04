{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Vandyland.BadgerState.Database(joinGroup, readDataFor, readGroup, readNDataFor, readSignalFor, writeData, writeSignal) where

import Control.Monad.Logger(NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Reader(ReaderT)
import Control.Monad.Trans.Resource(ResourceT)

import Data.Maybe(fromJust)
import Data.Time(diffUTCTime, getCurrentTime, UTCTime)
import Data.UUID(UUID)

import qualified Data.Text as Text
import qualified Data.UUID as UUID

import Database.Persist((=.), (==.), Entity(entityVal), insert, selectFirst, selectList, SelectOpt(Desc, LimitTo), upsert)
import Database.Persist.Postgresql(runMigration, runSqlPersistMPool, SqlBackend, withPostgresqlPool)
import Database.Persist.TH(mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import System.Random(randomIO)

import Vandyland.Common.DBCredentials(password, username)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
GroupDB
    groupID  Text
    bucketID Text
    dateAdded UTCTime
    Primary groupID bucketID dateAdded
    deriving Show
DataDB
    groupID   Text
    bucketID  Text
    dataT     Text
    dateAdded UTCTime
    Primary groupID bucketID dateAdded
    deriving Show
SignalDB
    groupID  Text
    bucketID Text
    signal   Text
    time     UTCTime
    Primary groupID bucketID
    UniqueSignalDB groupID bucketID
    deriving Show
|]

joinGroup :: Text -> IO UUID
joinGroup groupID = withDB $
  do
    uuid      <- liftIO randomIO
    timestamp <- liftIO getCurrentTime
    void $ insert $ GroupDB (Text.toLower groupID) (UUID.toText uuid) timestamp
    return uuid

readDataFor :: Text -> UUID -> UTCTime -> IO [(Text, UTCTime)]
readDataFor groupID bucketID refTime = withDB $
  do
    rows <- selectList [DataDBGroupID ==. (Text.toLower groupID), DataDBBucketID ==. (UUID.toText bucketID)] [Desc DataDBDateAdded]
    return $ rows >>= (entityVal &> (\(DataDB _ _ dataT time) -> if time `isNewerThan` refTime then [(dataT, time)] else []))

readNDataFor :: Text -> UUID -> Int -> IO [(Text, UTCTime)]
readNDataFor groupID bucketID n = withDB $
  do
    rows <- selectList [DataDBGroupID ==. (Text.toLower groupID), DataDBBucketID ==. (UUID.toText bucketID)] [Desc DataDBDateAdded, LimitTo n]
    return $ map (entityVal &> (\(DataDB _ _ dataT time) -> (dataT, time))) rows

readGroup :: Text -> IO [UUID]
readGroup groupID = withDB $
  do
    rows <- selectList [GroupDBGroupID ==. (Text.toLower groupID)] [Desc GroupDBDateAdded]
    return $ map (entityVal &> (\(GroupDB _ uuid _) -> fromJust $ UUID.fromText uuid)) rows

readSignalFor :: Text -> UUID -> IO (Maybe (Text, UTCTime))
readSignalFor groupID bucketID = withDB $
  do
    signalMaybe <- selectFirst [SignalDBGroupID ==. (Text.toLower groupID), SignalDBBucketID ==. (UUID.toText bucketID)] []
    return $ map (entityVal &> \(SignalDB _ _ signal timestamp) -> (signal, timestamp)) signalMaybe

writeData :: Text -> UUID -> Text -> IO UTCTime
writeData groupID bucketID dataT = withDB $
  do
    timestamp <- liftIO getCurrentTime
    void $ insert $ DataDB (Text.toLower groupID) (UUID.toText bucketID) dataT timestamp
    return timestamp

writeSignal :: Text -> UUID -> Text -> IO UTCTime
writeSignal groupID bucketID signal = withDB $
  do
    timestamp <- liftIO getCurrentTime
    let sig = SignalDB (Text.toLower groupID) (UUID.toText bucketID) signal timestamp
    void $ upsert sig [SignalDBSignal =. signal, SignalDBTime =. timestamp]
    return timestamp

isNewerThan :: UTCTime -> UTCTime -> Bool
isNewerThan x y = (diffUTCTime x y) > 0

withDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
withDB action = runNoLoggingT $ withPostgresqlPool connStr 50 $ \pool -> liftIO $
  do
    flip runSqlPersistMPool pool $
      do
        runMigration migrateAll
        action
  where
    connStr = "host=localhost dbname=vandyland user=" <> username <> " password=" <> password <> " port=5432"

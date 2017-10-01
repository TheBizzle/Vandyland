{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database(readCommentsFor, readSubmissionData, readSubmissionsLite, readSubmissionNames, writeComment, writeSubmission) where

import Bizzlelude

import Control.Monad.IO.Class(liftIO)
import Control.Monad.Logger(NoLoggingT, runNoLoggingT)
import Control.Monad.Trans.Reader(ReaderT)
import Control.Monad.Trans.Resource(ResourceT)

import Data.List(sortBy)
import Data.Ord(comparing)
import Data.Time(getCurrentTime, UTCTime)
import Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import Data.UUID(UUID)

import qualified Data.Text as Text
import qualified Data.UUID as UUID

import Database.Persist((<-.), (==.), Entity(entityVal), insert, selectFirst, selectList, SelectOpt(Asc))
import Database.Persist.Postgresql(runMigration, runSqlPersistMPool, SqlBackend, withPostgresqlPool)
import Database.Persist.TH(mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import System.Random(randomIO)

import Comment(Comment(Comment, time))
import DBCredentials(password, username)
import NameGen(generateName)
import Submission(Submission(Submission))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SubmissionDB
    sessionName Text
    uploadName  Text
    base64Image Text
    metadata    Text Maybe
    extraData   Text
    dateAdded   UTCTime
    Primary sessionName uploadName
    deriving Show
CommentDB
    uuid         Text
    comment      Text
    author       Text
    parent       Text Maybe
    sessionName  Text
    uploadName   Text
    time         UTCTime
    Primary uuid
    deriving Show
|]

readSubmissionNames :: Text -> IO [Text]
readSubmissionNames sessionName = withDB $
    do
      rows <- selectList [SubmissionDBSessionName ==. (Text.toLower sessionName)] [Asc SubmissionDBDateAdded]
      return $ map (entityVal >>> extractUploadName) rows

readSubmissionData :: Text -> Text -> IO (Maybe Text)
readSubmissionData sessionName uploadName = withDB $
    do
      sub <- selectFirst [SubmissionDBSessionName ==. (Text.toLower sessionName), SubmissionDBUploadName ==. (Text.toLower uploadName)] []
      return $ map (entityVal >>> extractData) sub

readSubmissionsLite :: Text -> [Text] -> IO [Submission]
readSubmissionsLite sessionName names = withDB $
    do
      subs <- selectList [SubmissionDBSessionName ==. (Text.toLower sessionName), SubmissionDBUploadName <-. (map Text.toLower names)] [Asc SubmissionDBDateAdded]
      return $ map (entityVal >>> dbToSubmission) subs

writeSubmission :: Text -> Text -> (Maybe Text) -> Text -> IO Text
writeSubmission sessionName imageBytes metadata extraData = withDB $
    do
      uploadName <- liftIO generateName
      timestamp  <- liftIO getCurrentTime
      let subDB = SubmissionDB (Text.toLower sessionName) (Text.toLower uploadName) imageBytes metadata extraData timestamp
      _ <- insert subDB
      return uploadName

readCommentsFor :: Text -> Text -> IO [Comment]
readCommentsFor sessionName uploadName = withDB $
    do
      rows <- selectList [CommentDBSessionName ==. (Text.toLower sessionName), CommentDBUploadName ==. (Text.toLower uploadName)] [Asc CommentDBTime]
      rows |> ((map $ entityVal >>> dbToComment) >>> (sortBy $ comparing time) >>> return)

writeComment :: Text -> Text -> Text -> Text -> Maybe UUID -> IO ()
writeComment comment uploadName sessionName author parent = withDB $
    do
      timestamp <- liftIO getCurrentTime
      uuid      <- liftIO randomIO
      let commentDB = CommentDB (UUID.toText uuid) comment author (map UUID.toText parent) (Text.toLower sessionName) (Text.toLower uploadName) timestamp
      _ <- insert commentDB
      return ()

withDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
withDB action = runNoLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $
  do
    flip runSqlPersistMPool pool $
      do
        runMigration migrateAll
        action
  where
    connStr = "host=localhost dbname=vandyland user=" <> username <> " password=" <> password <> " port=5432"

dbToSubmission :: SubmissionDB -> Submission
dbToSubmission (SubmissionDB _ uploadName image metadata _ _) = Submission uploadName image metadata

dbToComment :: CommentDB -> Comment
dbToComment (CommentDB uuid comment author parent _ _ time) = Comment uuid comment author parent (round $ (utcTimeToPOSIXSeconds time) * 1000)

extractUploadName :: SubmissionDB -> Text
extractUploadName (SubmissionDB _ uploadName _ _ _ _) = uploadName

extractData :: SubmissionDB -> Text
extractData (SubmissionDB _ _ _ _ extraData _) = extraData

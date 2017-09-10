{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database(readCommentsFor, readSubmissionsForSession, retrieveSubmissionData, retrieveSubmissionMetadata, writeComment, writeSubmission) where

import Bizzlelude

import Control.Monad.IO.Class(liftIO)

import Data.List(sortBy)
import Data.Ord(comparing)
import Data.Time(getCurrentTime, UTCTime)
import Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import Data.UUID(UUID)

import qualified Data.Text as Text
import qualified Data.UUID as UUID

import Database.Persist((==.), Entity(entityVal), insert, selectFirst, selectList)
import Database.Persist.Sqlite(runMigration, runSqlite)
import Database.Persist.TH(mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import System.Random(randomIO)

import Comment(Comment(Comment, time))
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

readSubmissionsForSession :: Text -> IO [Submission]
readSubmissionsForSession sessionName = runSqlite "vandyland.sqlite3" $
  do
    runMigration migrateAll
    rows <- selectList [SubmissionDBSessionName ==. (Text.toLower sessionName)] []
    return $ map (entityVal >>> dbToSubmission) rows

retrieveSubmissionMetadata :: Text -> Text -> IO (Maybe (Maybe Text))
retrieveSubmissionMetadata = extractFromSubmission extractMetadata

retrieveSubmissionData :: Text -> Text -> IO (Maybe Text)
retrieveSubmissionData = extractFromSubmission extractData

writeSubmission :: Text -> Text -> (Maybe Text) -> Text -> IO Text
writeSubmission sessionName imageBytes metadata extraData = runSqlite "vandyland.sqlite3" $
  do
    runMigration migrateAll
    uploadName <- liftIO generateName
    timestamp  <- liftIO getCurrentTime
    let subDB = SubmissionDB (Text.toLower sessionName) (Text.toLower uploadName) imageBytes metadata extraData timestamp
    _ <- insert subDB
    return uploadName

readCommentsFor :: Text -> Text -> IO [Comment]
readCommentsFor sessionName uploadName = runSqlite "vandyland.sqlite3" $
  do
    runMigration migrateAll
    rows      <- selectList [CommentDBSessionName ==. (Text.toLower sessionName), CommentDBUploadName ==. (Text.toLower uploadName)] []
    rows |> ((map $ entityVal >>> dbToComment) >>> (sortBy $ comparing time) >>> return)

writeComment :: Text -> Text -> Text -> Text -> Maybe UUID -> IO ()
writeComment comment uploadName sessionName author parent = runSqlite "vandyland.sqlite3" $
  do
    runMigration migrateAll
    timestamp <- liftIO getCurrentTime
    uuid      <- liftIO randomIO
    let commentDB = CommentDB (UUID.toText uuid) comment author (map UUID.toText parent) (Text.toLower sessionName) (Text.toLower uploadName) timestamp
    _ <- insert commentDB
    return ()

extractFromSubmission :: (SubmissionDB -> a) -> Text -> Text -> IO (Maybe a)
extractFromSubmission extract sessionName uploadName = runSqlite "vandyland.sqlite3" $
  do
    runMigration migrateAll
    sub <- selectFirst [SubmissionDBSessionName ==. (Text.toLower sessionName), SubmissionDBUploadName ==. (Text.toLower uploadName)] []
    return $ map (entityVal >>> extract) sub

dbToSubmission :: SubmissionDB -> Submission
dbToSubmission (SubmissionDB _ uploadName image _ _ _) = Submission uploadName image

dbToComment :: CommentDB -> Comment
dbToComment (CommentDB uuid comment author parent _ _ time) = Comment uuid comment author parent (round $ (utcTimeToPOSIXSeconds time) * 1000)

extractMetadata :: SubmissionDB -> Maybe Text
extractMetadata (SubmissionDB _ _ _ metadata _ _) = metadata

extractData :: SubmissionDB -> Text
extractData (SubmissionDB _ _ _ _ extraData _) = extraData

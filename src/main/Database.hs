{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database(readSubmissionsForSession, retrieveSubmissionData, writeSubmission) where

import Bizzlelude

import Data.Time(Day)

import qualified Data.Text as Text

import Database.Persist((==.), Entity(entityVal), insert, selectFirst, selectList)
import Database.Persist.Sqlite(runMigration, runSqlite)
import Database.Persist.TH(mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import Submission(Submission(Submission))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SubmissionDB
    sessionName Text
    uploadName  Text
    base64Image Text
    extraData   Text
    dateAdded   Day
    Primary sessionName uploadName
    deriving Show
|]

readSubmissionsForSession :: Text -> IO [Submission]
readSubmissionsForSession sessionName = runSqlite "vandyland.sqlite3" $
  do
    runMigration migrateAll
    rows <- selectList [SubmissionDBSessionName ==. (Text.toLower sessionName)] []
    return $ map (entityVal >>> dbToSubmission) rows

retrieveSubmissionData :: Text -> Text -> IO (Maybe Text)
retrieveSubmissionData sessionName uploadName = runSqlite "vandyland.sqlite3" $
  do
    runMigration migrateAll
    sub <- selectFirst [SubmissionDBSessionName ==. (Text.toLower sessionName), SubmissionDBUploadName ==. (Text.toLower uploadName)] []
    return $ map (entityVal >>> extractData) sub

writeSubmission :: Day -> Text -> Text -> Text -> Text -> IO ()
writeSubmission timestamp uploadName sessionName imageBytes extraData = runSqlite "vandyland.sqlite3" $
  do
    runMigration migrateAll
    let subDB = SubmissionDB (Text.toLower sessionName) (Text.toLower uploadName) imageBytes extraData timestamp
    _ <- insert subDB
    return ()

dbToSubmission :: SubmissionDB -> Submission
dbToSubmission (SubmissionDB _ uploadName image _ _) = Submission uploadName image

extractData :: SubmissionDB -> Text
extractData (SubmissionDB _ _ _ extraData _) = extraData

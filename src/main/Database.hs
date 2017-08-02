{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database(writeSubmission) where

import Bizzlelude

import Data.ByteString(ByteString)
import Data.Time(Day)

import Database.Persist((==.), Entity(entityVal), insert, selectFirst, selectList)
import Database.Persist.Sqlite(runMigration, runSqlite)
import Database.Persist.TH(mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import Submission(Submission(Submission))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SubmissionDB
    sessionName Text
    uploadName  Text
    imageBytes  ByteString
    extraData   Text
    dateAdded   Day
    Primary sessionName uploadName
    deriving Show
|]

writeSubmission :: Day -> Text -> Submission -> IO ()
writeSubmission timestamp uploadName (Submission sessionName imageBytes extraData) = runSqlite "vandyland.sqlite3" $
  do
    runMigration migrateAll
    let subDB = SubmissionDB sessionName uploadName imageBytes extraData timestamp
    _ <- insert subDB
    return ()

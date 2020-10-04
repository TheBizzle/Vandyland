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

module Vandyland.Gallery.Database(SuppressionResult(Suppressed, NotAuthorized, NotFound), readCommentsFor, readSubmissionData, readSubmissionsLite, readSubmissionListings, suppressSubmission, uniqueSessionName, writeComment, writeSubmission) where

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

import Database.Persist((<-.), (=.), (==.), Entity(entityKey, entityVal), insert, selectFirst, selectList, SelectOpt(Asc), update)
import Database.Persist.Postgresql(runMigration, runSqlPersistMPool, SqlBackend, withPostgresqlPool)
import Database.Persist.TH(mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import System.Random(randomIO)

import Vandyland.Common.DBCredentials(password, username)

import Vandyland.Gallery.Comment(Comment(Comment, time))
import Vandyland.Gallery.NameGen(generateName)
import Vandyland.Gallery.Submission(Submission(Submission), SubmissionListing(SubmissionListing))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SubmissionDB
    sessionName  Text
    uploadName   Text
    base64Image  Text
    authorToken  Text Maybe
    isSuppressed Bool
    metadata     Text Maybe
    extraData    Text
    dateAdded    UTCTime
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

uniqueSessionName :: IO Text
uniqueSessionName = withDB $
  do
    name       <- liftIO generateName
    entryMaybe <- selectFirst [SubmissionDBSessionName ==. (Text.toLower name)] []
    if isJust entryMaybe then liftIO uniqueSessionName else return name

uniqueSubmissionName :: Text -> IO Text
uniqueSubmissionName sessionName = withDB $
  do
    name       <- liftIO generateName
    entryMaybe <- selectFirst [SubmissionDBSessionName ==. (Text.toLower sessionName), SubmissionDBUploadName ==. (Text.toLower name)] []
    if isJust entryMaybe then liftIO (uniqueSubmissionName sessionName) else return name

readSubmissionListings :: Text -> IO [SubmissionListing]
readSubmissionListings sessionName = withDB $
    do
      rows <- selectList [SubmissionDBSessionName ==. (Text.toLower sessionName)] [Asc SubmissionDBDateAdded]
      return $ map (entityVal &> dbToSubListing) rows

readSubmissionData :: Text -> Text -> IO (Maybe Text)
readSubmissionData sessionName uploadName = withDB $
    do
      sub <- selectFirst [SubmissionDBSessionName ==. (Text.toLower sessionName), SubmissionDBUploadName ==. (Text.toLower uploadName)] []
      return $ map (entityVal &> extractData) sub

readSubmissionsLite :: Text -> [Text] -> IO [Submission]
readSubmissionsLite sessionName names = withDB $
    do
      subs <- selectList [SubmissionDBSessionName ==. (Text.toLower sessionName), SubmissionDBUploadName <-. (map Text.toLower names)] [Asc SubmissionDBDateAdded]
      return $ map (entityVal &> dbToSubmission) subs

suppressSubmission :: Text -> Text -> (Maybe UUID) -> IO SuppressionResult
suppressSubmission _           _          Nothing   = return NotAuthorized
suppressSubmission sessionName uploadName tokenJust = withDB $
  do
    sub <- selectFirst [SubmissionDBSessionName ==. (Text.toLower sessionName), SubmissionDBUploadName ==. (Text.toLower uploadName)] []
    maybe (return NotFound) (\s -> if tokenJust == (extractToken $ entityVal s) then liftIO (suppressIt $ entityKey s) else return NotAuthorized) sub
  where
    suppressIt subKey = withDB $
      do
        void $ update subKey [SubmissionDBIsSuppressed =. True]
        return Suppressed

writeSubmission :: Text -> Text -> (Maybe Text) -> (Maybe Text) -> Text -> IO Text
writeSubmission sessionName imageBytes token metadata extraData = withDB $
    do
      uploadName <- liftIO $ uniqueSubmissionName sessionName
      timestamp  <- liftIO getCurrentTime
      let subDB = SubmissionDB (Text.toLower sessionName) (Text.toLower uploadName) imageBytes token False metadata extraData timestamp
      void $ insert subDB
      return uploadName

readCommentsFor :: Text -> Text -> IO [Comment]
readCommentsFor sessionName uploadName = withDB $
    do
      rows <- selectList [CommentDBSessionName ==. (Text.toLower sessionName), CommentDBUploadName ==. (Text.toLower uploadName)] [Asc CommentDBTime]
      rows |> (map $ entityVal &> dbToComment) &> (sortBy $ comparing time) &> return

writeComment :: Text -> Text -> Text -> Text -> Maybe UUID -> IO ()
writeComment comment uploadName sessionName author parent = withDB $
    do
      timestamp <- liftIO getCurrentTime
      uuid      <- liftIO randomIO
      let commentDB = CommentDB (UUID.toText uuid) comment author (map UUID.toText parent) (Text.toLower sessionName) (Text.toLower uploadName) timestamp
      void $ insert commentDB
      return ()

withDB :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
withDB action = runNoLoggingT $ withPostgresqlPool connStr 50 $ \pool -> liftIO $
  do
    flip runSqlPersistMPool pool $
      do
        runMigration migrateAll -- We do this for every DB transaction?  Major performance issue at scale, I'd expect. --JAB (10/4/20)
        action
  where
    connStr = "host=localhost dbname=vandyland user=" <> username <> " password=" <> password <> " port=5432"

dbToSubListing :: SubmissionDB -> SubmissionListing
dbToSubListing (SubmissionDB _ uploadName _ _ isSuppressed _ _ _) = SubmissionListing uploadName isSuppressed

dbToSubmission :: SubmissionDB -> Submission
dbToSubmission (SubmissionDB _ uploadName image token _ metadata _ _) = Submission uploadName image (token >>= UUID.fromText) metadata

dbToComment :: CommentDB -> Comment
dbToComment (CommentDB uuid comment author parent _ _ time) = Comment uuid comment author parent (round $ (utcTimeToPOSIXSeconds time) * 1000)

extractToken :: SubmissionDB -> Maybe UUID
extractToken (SubmissionDB _ _ _ token _ _ _ _) = token >>= UUID.fromText

extractData :: SubmissionDB -> Text
extractData (SubmissionDB _ _ _ _ _ _ extraData _) = extraData

data SuppressionResult
  = Suppressed
  | NotAuthorized
  | NotFound

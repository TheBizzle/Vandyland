{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Vandyland.Gallery.Database(approveSubmission, forbidSubmission, PrivilegedActionResult(Fulfilled, NotAuthorized, NotFound), readCommentsFor, readGalleryListings, readSessionExists, readStarterConfigFor, readSubmissionData, readSubmissionsLite, readSubmissionListings, readSubmissionListingsForModeration, readTemplateName, registerNewSession, suppressSubmission, uniqueSessionName, writeComment, writeSubmission) where

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

import Database.Persist((<-.), (=.), (==.), count, Entity(entityKey, entityVal), insert, selectFirst, selectList, SelectOpt(Asc), update)
import Database.Persist.Postgresql(runMigration, runSqlPersistMPool, SqlBackend, withPostgresqlPool)
import Database.Persist.TH(mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import System.Random(randomIO)

import Vandyland.Common.DBCredentials(password, username)

import Vandyland.Gallery.Comment(Comment(Comment, time))
import Vandyland.Gallery.NameGen(generateName)
import Vandyland.Gallery.Submission(GalleryListing(GalleryListing), Submission(Submission), SubmissionListing(SubmissionListing))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
GalleryDB
    galleryName     Text
    templateName    Text
    ownerToken      Text Maybe
    getsPrescreened Bool
    config          Text Maybe
    description     Text
    dateAdded       UTCTime
    Primary galleryName
    deriving Show
SubmissionDB
    sessionName          Text
    uploadName           Text
    base64Image          Text
    authorToken          Text Maybe
    isSuppressed         Bool
    isForbidden          Bool
    isAwaitingModeration Bool
    metadata             Text Maybe
    extraData            Text
    dateAdded            UTCTime
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
    case entryMaybe of
      Nothing  -> return name
      (Just _) -> liftIO uniqueSessionName

uniqueSubmissionName :: Text -> IO Text
uniqueSubmissionName sessionName = withDB $
  do
    name       <- liftIO generateName
    entryMaybe <- selectFirst [SubmissionDBSessionName ==. (Text.toLower sessionName), SubmissionDBUploadName ==. (Text.toLower name)] []
    case entryMaybe of
      Nothing  -> return name
      (Just _) -> liftIO $ uniqueSubmissionName sessionName

registerNewSession :: Text -> Text -> Bool -> Maybe Text -> Text -> Maybe UUID -> IO Bool
registerNewSession template name getsPrescreened configMaybe description tokenMaybe = withDB $
  do
    entityMaybe <- selectFirst [GalleryDBGalleryName ==. name] []
    rows        <- selectList  [SubmissionDBSessionName ==. (Text.toLower name)] []
    if isJust entityMaybe || (not . null) rows then
      return False
    else
      insertIt >> (return True)
  where
    insertIt =
      do
        timestamp <- liftIO getCurrentTime
        let tokey  = map UUID.toText tokenMaybe
        insert $ GalleryDB (Text.toLower name) (Text.toLower template) tokey getsPrescreened configMaybe description timestamp

readGalleryListings :: UUID -> IO [GalleryListing]
readGalleryListings token = withDB $
    do
      rows <- selectList [GalleryDBOwnerToken ==. (Just $ UUID.toText token)] [Asc GalleryDBDateAdded]
      let particles = map (entityVal &> dbToGalListingParticle) rows
      flip mapM particles $ \(name, template, isPre, desc, cDate) -> liftIO $ withDB $ do
        numWaiting  <- count [SubmissionDBSessionName ==. (Text.toLower name), SubmissionDBIsAwaitingModeration ==. True]
        rows        <- selectList [ SubmissionDBSessionName          ==. (Text.toLower name)
                                  , SubmissionDBIsAwaitingModeration ==. False
                                  , SubmissionDBIsForbidden          ==. False
                                  ] [Asc SubmissionDBDateAdded]
        let uploads     = map entityVal rows
        let numApproved = length uploads
        let cTime       = asPOSIX cDate
        let lTime       = getMax cTime uploads
        return $ GalleryListing name template desc isPre numWaiting numApproved cTime lTime
    where
      getMax initTime = (map extractSubDateAdded) >>> (foldr chooseLater initTime)
      chooseLater a b = if a < b then b else a

readSubmissionListings :: Text -> IO [SubmissionListing]
readSubmissionListings sessionName = withDB $
    do
      rows <- selectList [ SubmissionDBSessionName          ==. (Text.toLower sessionName)
                         , SubmissionDBIsAwaitingModeration ==. False
                         , SubmissionDBIsForbidden          ==. False
                         ] [Asc SubmissionDBDateAdded]
      return $ map (entityVal &> dbToSubListing) rows

readSessionExists :: Text -> IO Bool
readSessionExists sessionName = withDB $
    do
      gEntityMaybe <- selectFirst [GalleryDBGalleryName    ==. (Text.toLower sessionName)] []
      sEntityMaybe <- selectFirst [SubmissionDBSessionName ==. (Text.toLower sessionName)] []
      return $ (isJust gEntityMaybe || isJust sEntityMaybe)

readSubmissionListingsForModeration :: Text -> UUID -> IO (PrivilegedActionResult [Text])
readSubmissionListingsForModeration sessionName token = withDB $
    do
      entityMaybe <- selectFirst [GalleryDBGalleryName ==. (Text.toLower sessionName)] []
      case entityMaybe of
        Nothing       -> return NotFound
        (Just entity) -> liftIO $ withDB $ do
          if (Just token) == (entity |> entityVal &> extractOwnerToken) then do
            rows <- selectList [SubmissionDBSessionName ==. (Text.toLower sessionName), SubmissionDBIsAwaitingModeration ==. True] [Asc SubmissionDBDateAdded]
            return $ Fulfilled $ map (entityVal &> extractUploadName) rows
          else
            return NotAuthorized

readSubmissionData :: Text -> Text -> Maybe UUID -> IO (PrivilegedActionResult Text)
readSubmissionData sessionName uploadName tokenMaybe = withDB $
    do
      sEntityMaybe <- selectFirst [SubmissionDBSessionName ==. (Text.toLower sessionName), SubmissionDBUploadName ==. (Text.toLower uploadName)] []
      gEntityMaybe <- selectFirst [GalleryDBGalleryName ==. (Text.toLower sessionName)] []
      let teacherTokenMaybe = (gEntityMaybe >>= (entityVal &> extractOwnerToken))
      maybe (return NotFound) (entityVal &> retrieveSubmission extractData tokenMaybe teacherTokenMaybe &> liftIO) sEntityMaybe

readSubmissionsLite :: Text -> Maybe UUID -> [Text] -> IO [Submission]
readSubmissionsLite sessionName tokenMaybe names = withDB $
    do
      entities  <- selectList [SubmissionDBSessionName ==. (Text.toLower sessionName), SubmissionDBUploadName <-. (map Text.toLower names)] [Asc SubmissionDBDateAdded]
      let subs   = map entityVal entities
      validSubs <- flip mapM subs $ \sub -> liftIO $ withDB $ do
        gEntityMaybe         <- selectFirst [GalleryDBGalleryName ==. (extractSessionName sub)] []
        let teacherTokenMaybe = gEntityMaybe >>= (entityVal &> extractOwnerToken)
        liftIO $ retrieveSubmission (dbToSubmission teacherTokenMaybe) tokenMaybe teacherTokenMaybe sub
      return $ validSubs >>= collectFulfilled
  where
    collectFulfilled (Fulfilled x) = [x]
    collectFulfilled _             = []

readTemplateName :: Text -> IO (Maybe Text)
readTemplateName sessionName = withDB $
  do
    entityMaybe <- selectFirst [GalleryDBGalleryName ==. (Text.toLower sessionName)] []
    return $ map (entityVal >>> extractTemplateName) entityMaybe

suppressSubmission :: Text -> Text -> UUID -> IO (PrivilegedActionResult ())
suppressSubmission sessionName uploadName token = withDB $
  do
    entityMaybe  <- selectFirst [SubmissionDBSessionName ==. (Text.toLower sessionName), SubmissionDBUploadName ==. (Text.toLower uploadName)] []
    galleryMaybe <- selectFirst [GalleryDBGalleryName ==. (Text.toLower sessionName)] []
    case entityMaybe of
      Nothing       -> return NotFound
      (Just entity) -> do
        let isUploader  = (Just token) == (entity |> entityVal &> extractToken)
        let isModerator = (Just token) == (galleryMaybe >>= (entityVal &> extractOwnerToken))
        if isUploader || isModerator then do
          void $ update (entityKey entity) [SubmissionDBIsSuppressed =. True]
          return $ Fulfilled ()
        else
          return NotAuthorized

forbidSubmission :: Text -> Text -> UUID -> IO (PrivilegedActionResult ())
forbidSubmission = moderateSubmission True

approveSubmission :: Text -> Text -> UUID -> IO (PrivilegedActionResult ())
approveSubmission = moderateSubmission False

moderateSubmission :: Bool -> Text -> Text -> UUID -> IO (PrivilegedActionResult ())
moderateSubmission isForbidden sessionName uploadName token = withDB $
  do
    entityMaybe <- selectFirst [SubmissionDBSessionName ==. (Text.toLower sessionName), SubmissionDBUploadName ==. (Text.toLower uploadName)] []
    case entityMaybe of
      Nothing       -> return NotFound
      (Just entity) -> do
        gEntityMaybe <- selectFirst [GalleryDBGalleryName ==. (Text.toLower sessionName)] []
        if (Just token) == (gEntityMaybe >>= (entityVal &> extractOwnerToken)) then do
          void $ update (entityKey entity) [SubmissionDBIsForbidden          =. isForbidden]
          void $ update (entityKey entity) [SubmissionDBIsAwaitingModeration =. False]
          return $ Fulfilled ()
        else
          return NotAuthorized

writeSubmission :: Text -> Text -> Maybe UUID -> Maybe Text -> Text -> IO Text
writeSubmission sessionName imageBytes tokenMaybe metadata extraData = withDB $
    do
      uploadName   <- liftIO $ uniqueSubmissionName sessionName
      timestamp    <- liftIO getCurrentTime
      gEntityMaybe <- selectFirst [GalleryDBGalleryName ==. (Text.toLower sessionName)] []
      let tokey     = map UUID.toText tokenMaybe
      let getsPreed = maybe False (entityVal &> extractGetsPrescreened) gEntityMaybe
      let subDB     = SubmissionDB (Text.toLower sessionName) (Text.toLower uploadName) imageBytes tokey False False getsPreed metadata extraData timestamp
      void $ insert subDB
      return uploadName

readStarterConfigFor :: Text -> IO (Maybe (Maybe Text))
readStarterConfigFor galleryName = withDB $
  do
    entryMaybe <- selectFirst [GalleryDBGalleryName ==. (Text.toLower galleryName)] []
    return $ map (entityVal &> extractStarterConfig) entryMaybe

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
        --runMigration migrateAll -- We do this for every DB transaction?  Major performance issue at scale, I'd expect. --JAB (10/4/20)
        action
  where
    connStr = "host=localhost dbname=vandyland user=" <> username <> " password=" <> password <> " port=5432"

retrieveSubmission :: (SubmissionDB -> a) -> Maybe UUID -> Maybe UUID -> SubmissionDB -> IO (PrivilegedActionResult a)
retrieveSubmission f givenTokenMaybe teacherTokenMaybe submission = withDB $
    do
      let authorTokenMaybe = extractToken           submission
      let needsModeration  = extractNeedsModeration submission
      let isSuppressed     = extractIsSuppressed    submission
      return $
        if givenTokenMaybe == authorTokenMaybe || givenTokenMaybe == teacherTokenMaybe || ((not needsModeration) && (not isSuppressed)) then
          Fulfilled $ f submission
        else
          NotAuthorized

extractTemplateName :: GalleryDB -> Text
extractTemplateName (GalleryDB _ tn _ _ _ _ _) = tn

extractOwnerToken :: GalleryDB -> Maybe UUID
extractOwnerToken (GalleryDB _ _ otm _ _ _ _) = otm >>= UUID.fromText

extractGetsPrescreened :: GalleryDB -> Bool
extractGetsPrescreened (GalleryDB _ _ _ gp _ _ _) = gp

extractStarterConfig :: GalleryDB -> Maybe Text
extractStarterConfig (GalleryDB _ _ _ _ sc _ _) = sc

dbToGalListingParticle :: GalleryDB -> (Text, Text, Bool, Text, UTCTime)
dbToGalListingParticle (GalleryDB gn tp _ gp _ de da) = (gn, tp, gp, de, da)

dbToSubListing :: SubmissionDB -> SubmissionListing
dbToSubListing (SubmissionDB _ uploadName _ _ isSuppressed _ _ _ _ _) = SubmissionListing uploadName isSuppressed

dbToSubmission :: Maybe UUID -> SubmissionDB -> Submission
dbToSubmission ownerToken (SubmissionDB _ uploadName image token _ _ _ metadata _ _) =
  Submission uploadName image (token >>= UUID.fromText) ownerToken metadata

dbToComment :: CommentDB -> Comment
dbToComment (CommentDB uuid comment author parent _ _ time) = Comment uuid comment author parent (asPOSIX time)

extractSessionName :: SubmissionDB -> Text
extractSessionName (SubmissionDB sn _ _ _ _ _ _ _ _ _) = sn

extractUploadName :: SubmissionDB -> Text
extractUploadName (SubmissionDB _ un _ _ _ _ _ _ _ _) = un

extractToken :: SubmissionDB -> Maybe UUID
extractToken (SubmissionDB _ _ _ token _ _ _ _ _ _) = token >>= UUID.fromText

extractIsSuppressed :: SubmissionDB -> Bool
extractIsSuppressed (SubmissionDB _ _ _ _ isSuppressed _ _ _ _ _) = isSuppressed

extractNeedsModeration :: SubmissionDB -> Bool
extractNeedsModeration (SubmissionDB _ _ _ _ _ _ needsModeration _ _ _) = needsModeration

extractData :: SubmissionDB -> Text
extractData (SubmissionDB _ _ _ _ _ _ _ _ extraData _) = extraData

extractSubDateAdded :: SubmissionDB -> Integer
extractSubDateAdded (SubmissionDB _ _ _ _ _ _ _ _ _ dateAdded) = asPOSIX dateAdded

asPOSIX :: UTCTime -> Integer
asPOSIX = utcTimeToPOSIXSeconds >>> (* 1000) >>> round

data PrivilegedActionResult a
  = Fulfilled a
  | NotAuthorized
  | NotFound

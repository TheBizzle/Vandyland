{-# LANGUAGE TupleSections #-}
module Vandyland.Common.SnapHelpers(allowingCORS, Arg(Arg), asInt, asNonNegInt, asUUID, Constraint(Constraint), decodeText, encodeText, failWith, free, getParamV, handle1, handle2, handle3, handle4, handle5, nonEmpty, notifyBadParams, succeed, withFileUploads) where

import Codec.Compression.GZip(decompress)
import Codec.Compression.Zlib.Internal(DecompressError)

import Control.Exception(catch)
import Control.Lens((#))

import Data.Aeson(decode, encode, FromJSON, ToJSON)
import Data.Bifoldable(bimapM_)
import Data.ByteString(ByteString)
import Data.Text.Encoding(decodeUtf8, encodeUtf8)
import Data.Text.IO(readFile)
import Data.UUID(UUID)
import Data.Validation(_Failure, _Success, Validation(Success, Failure))

import Snap.Core(getParam, method, Method, modifyResponse, setContentType, setResponseStatus, Snap, writeText)
import Snap.Util.CORS(applyCORS, defaultOptions)
import Snap.Util.FileUploads(allowWithMaximumSize, defaultUploadPolicy, handleFileUploads, PartInfo(partFileName), PolicyViolationException, policyViolationExceptionReason)

import System.Directory(createDirectoryIfMissing)

import Text.Read(readMaybe)

import qualified Data.ByteString.Lazy    as LazyByteString
import qualified Data.Map                as Map
import qualified Data.Text.Lazy          as LazyText
import qualified Data.Text.Lazy.Encoding as LazyTextEncoding
import qualified Data.UUID               as UUID

data Constraint t =
  Constraint (ByteString -> Text -> Validation [Text] t)

data Arg t =
  Arg ByteString (Constraint t)

handle1 :: Arg t -> (t -> Snap ()) -> Snap ()
handle1 arg onSuccess =
  do
    arg <- getParamV arg
    bimapM_ notifyBadParams onSuccess arg

handle2 :: (Arg t1, Arg t2) -> ((t1, t2) -> Snap ()) -> Snap ()
handle2 (arg1, arg2) onSuccess =
  do
    arg1 <- getParamV arg1
    arg2 <- getParamV arg2
    let tupleV = (,) <$> arg1 <*> arg2
    bimapM_ notifyBadParams onSuccess tupleV

handle3 :: (Arg t1, Arg t2, Arg t3) -> ((t1, t2, t3) -> Snap ()) -> Snap ()
handle3 (arg1, arg2, arg3) onSuccess =
  do
    arg1 <- getParamV arg1
    arg2 <- getParamV arg2
    arg3 <- getParamV arg3
    let tupleV = (,,) <$> arg1 <*> arg2 <*> arg3
    bimapM_ notifyBadParams onSuccess tupleV

handle4 :: (Arg t1, Arg t2, Arg t3, Arg t4) -> ((t1, t2, t3, t4) -> Snap ()) -> Snap ()
handle4 (arg1, arg2, arg3, arg4) onSuccess =
  do
    arg1 <- getParamV arg1
    arg2 <- getParamV arg2
    arg3 <- getParamV arg3
    arg4 <- getParamV arg4
    let tupleV = (,,,) <$> arg1 <*> arg2 <*> arg3 <*> arg4
    bimapM_ notifyBadParams onSuccess tupleV

handle5 :: (Arg t1, Arg t2, Arg t3, Arg t4, Arg t5) -> ((t1, t2, t3, t4, t5) -> Snap ()) -> Snap ()
handle5 (arg1, arg2, arg3, arg4, arg5) onSuccess =
  do
    arg1 <- getParamV arg1
    arg2 <- getParamV arg2
    arg3 <- getParamV arg3
    arg4 <- getParamV arg4
    arg5 <- getParamV arg5
    let tupleV = (,,,,) <$> arg1 <*> arg2 <*> arg3 <*> arg4 <*> arg5
    bimapM_ notifyBadParams onSuccess tupleV

decodeText :: FromJSON a => Text -> Maybe a
decodeText = encodeUtf8 &> LazyByteString.fromStrict &> decode

encodeText :: ToJSON a => a -> Text
encodeText = encode &> LazyTextEncoding.decodeUtf8 &> LazyText.toStrict

getParamV :: Arg to -> Snap (Validation [Text] to)
getParamV (Arg paramName (Constraint constrain)) =
  do
    param <- getParam paramName
    let paramV = maybe (_Failure # [decodeUtf8 paramName]) (\x -> _Success # (decodeUtf8 x)) param
    return $ case paramV of
               (Success value) -> constrain paramName value
               (Failure errs)  -> _Failure # errs

allowingCORS :: Method -> Snap () -> Snap ()
allowingCORS mthd f = applyCORS defaultOptions $ method mthd f

notifyBadParams :: [Text] -> Snap ()
notifyBadParams = (map ("Missing/invalid parameter: " <>)) &> unlines &> writeText &> (failWith 422)

failWith :: Int -> Snap () -> Snap ()
failWith x snap =
  do
    modifyResponse $ setResponseStatus x $ statusName x
    snap
  where
    statusName 400 = "Bad Request"
    statusName 404 = "Not Found"
    statusName 422 = "Unprocessable Entity"
    statusName _   = error "Unhandled status"

succeed :: ByteString -> Text -> Snap ()
succeed contentType output =
  do
    modifyResponse $ setContentType contentType
    writeText output

asInt :: Constraint Int
asInt = Constraint $ \paramName x -> (readMaybe (asString x) :: Maybe Int) |> (maybe (_Failure # [(decodeUtf8 paramName)]) (_Success #))

asNonNegInt :: Constraint Int
asNonNegInt = Constraint $ \paramName x -> (readMaybe (asString x) :: Maybe Int) |>
  (>>= (\n -> if n < 0 then Nothing else Just n)) &> maybe (_Failure # [(decodeUtf8 paramName)]) (_Success #)

asUUID :: Constraint UUID
asUUID = Constraint $ \paramName x -> (UUID.fromText x) |> maybe (_Failure # [(decodeUtf8 paramName)]) (_Success #)

free :: Constraint Text
free = Constraint $ \_ x -> _Success # x

nonEmpty :: Constraint Text
nonEmpty = Constraint $ (\paramName x -> case x of
                                              "" -> _Failure # [(decodeUtf8 paramName) <> " cannot be empty"]
                                              y  -> _Success # y)

withFileUploads :: (Map Text Text -> Snap ()) -> Snap ()
withFileUploads f = (withFileUploadsHelper "dist/filetmp") >>= (bimapM_ (unlines &> writeText &> failWith 400) f)

withFileUploadsHelper :: FilePath -> Snap (Validation [Text] (Map Text Text))
withFileUploadsHelper directory =
  do
    liftIO $ createDirectoryIfMissing True directory
    fileMappingVs <- handleFileUploads directory defaultUploadPolicy (const $ allowWithMaximumSize 20000000) handleRead
    fileMappingVs |> sequenceV &> (map Map.fromList) &> return
  where
    sequenceV :: [Validation a b] -> Validation [a] [b]
    sequenceV = foldr helper (_Success # [])
      where
        helper :: Validation a b -> Validation [a] [b] -> Validation [a] [b]
        helper (Success s) (    (Success ss)) = _Success # (s:ss)
        helper (Failure f) (    (Failure fs)) = _Failure # (f:fs)
        helper (Failure f) (    (Success  _)) = _Failure #    [f]
        helper (Success _) (res@(Failure  _)) = res
    handleRead :: PartInfo -> (Either PolicyViolationException FilePath) -> IO (Validation Text (Text, Text))
    handleRead partInfo = either lefty righty
      where
        key    = partInfo |> partFileName &> (fromMaybe "-") &> decodeUtf8
        lefty  = policyViolationExceptionReason  &> (_Failure #) &> return
        righty = readPossibleGZip &> liftIO &>= ((key,) &> (_Success #) &> return)
          where
            readPossibleGZip filepath = catch (readGZip filepath) (\e -> const (readFile filepath) (e :: DecompressError))
              where
                readGZip = LazyByteString.readFile &>= (decompress &> LazyTextEncoding.decodeUtf8 &> LazyText.toStrict &> return')

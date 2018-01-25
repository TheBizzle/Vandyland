module SnapHelpers(allowingCORS, Constraint(NonEmpty), decodeText, encodeText, failWith, getParamV, handle1, handle2, handle3, handle4, handle5, notifyBadParams, succeed, withFileUploads) where

import Codec.Compression.GZip(decompress)
import Codec.Compression.Zlib.Internal(DecompressError)

import Control.Exception(catch)
import Control.Lens((#))
import Control.Monad.IO.Class(liftIO)

import Data.Aeson(decode, encode, FromJSON, ToJSON)
import Data.Bifoldable(bimapM_)
import Data.ByteString(ByteString)
import Data.Text.Encoding(decodeUtf8, encodeUtf8)
import Data.Text.IO(readFile)
import Data.Validation(_Failure, _Success, AccValidation(AccSuccess, AccFailure))

import Snap.Core(getParam, method, Method, modifyResponse, setContentType, setResponseStatus, Snap, writeText)
import Snap.CORS(applyCORS, defaultOptions)
import Snap.Util.FileUploads(allowWithMaximumSize, defaultUploadPolicy, handleFileUploads, PartInfo(partFileName), PolicyViolationException, policyViolationExceptionReason)

import System.Directory(createDirectoryIfMissing)

import qualified Data.ByteString.Lazy    as LazyByteString
import qualified Data.Map                as Map
import qualified Data.Text.Lazy          as LazyText
import qualified Data.Text.Lazy.Encoding as LazyTextEncoding

data Constraint
  = NonEmpty

type Arg = (ByteString, [Constraint])

handle1 :: Arg -> (Text -> Snap ()) -> Snap ()
handle1 arg onSuccess =
  do
    arg <- getParamV arg
    bimapM_ notifyBadParams onSuccess arg

handle2 :: (Arg, Arg) -> ((Text, Text) -> Snap ()) -> Snap ()
handle2 (arg1, arg2) onSuccess =
  do
    arg1 <- getParamV arg1
    arg2 <- getParamV arg2
    let tupleV = (,) <$> arg1 <*> arg2
    bimapM_ notifyBadParams onSuccess tupleV

handle3 :: (Arg, Arg, Arg) -> ((Text, Text, Text) -> Snap ()) -> Snap ()
handle3 (arg1, arg2, arg3) onSuccess =
  do
    arg1 <- getParamV arg1
    arg2 <- getParamV arg2
    arg3 <- getParamV arg3
    let tupleV = (,,) <$> arg1 <*> arg2 <*> arg3
    bimapM_ notifyBadParams onSuccess tupleV

handle4 :: (Arg, Arg, Arg, Arg) -> ((Text, Text, Text, Text) -> Snap ()) -> Snap ()
handle4 (arg1, arg2, arg3, arg4) onSuccess =
  do
    arg1 <- getParamV arg1
    arg2 <- getParamV arg2
    arg3 <- getParamV arg3
    arg4 <- getParamV arg4
    let tupleV = (,,,) <$> arg1 <*> arg2 <*> arg3 <*> arg4
    bimapM_ notifyBadParams onSuccess tupleV

handle5 :: (Arg, Arg, Arg, Arg, Arg) -> ((Text, Text, Text, Text, Text) -> Snap ()) -> Snap ()
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
decodeText = encodeUtf8 >>> LazyByteString.fromStrict >>> decode

encodeText :: ToJSON a => a -> Text
encodeText = encode >>> LazyTextEncoding.decodeUtf8 >>> LazyText.toStrict

getParamV :: Arg -> Snap (AccValidation [Text] Text)
getParamV (paramName, constraints) =
  do
    param <- getParam paramName
    let deconstrained = deconstrain constraints param
    return $ maybe (_Failure # [decodeUtf8 paramName]) (\x -> _Success # (decodeUtf8 x)) deconstrained
  where
    deconstrain _              Nothing = Nothing
    deconstrain []                   x = x
    deconstrain (NonEmpty:_) (Just "") = Nothing
    deconstrain (NonEmpty:t)         x = deconstrain t x

allowingCORS :: Method -> Snap () -> Snap ()
allowingCORS mthd f = applyCORS defaultOptions $ method mthd f

notifyBadParams :: [Text] -> Snap ()
notifyBadParams = (map ("Missing parameter: " <>)) >>> unlines >>> writeText >>> (failWith 422)

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

withFileUploads :: (Map Text Text -> Snap ()) -> Snap ()
withFileUploads f = (withFileUploadsHelper "dist/filetmp") >>= (bimapM_ (unlines >>> writeText >>> failWith 400) f)

withFileUploadsHelper :: FilePath -> Snap (AccValidation [Text] (Map Text Text))
withFileUploadsHelper directory =
  do
    liftIO $ createDirectoryIfMissing True directory
    fileMappingVs <- handleFileUploads directory defaultUploadPolicy (const $ allowWithMaximumSize 20000000) handleRead
    fileMappingVs |> (sequenceV >>> (map Map.fromList) >>> return)
  where
    sequenceV :: [AccValidation a b] -> AccValidation [a] [b]
    sequenceV = foldr helper (_Success # [])
      where
        helper :: AccValidation a b -> AccValidation [a] [b] -> AccValidation [a] [b]
        helper (AccSuccess s) (    (AccSuccess ss)) = _Success # (s:ss)
        helper (AccFailure f) (    (AccFailure fs)) = _Failure # (f:fs)
        helper (AccFailure f) (    (AccSuccess  _)) = _Failure #    [f]
        helper (AccSuccess _) (res@(AccFailure  _)) = res
    handleRead :: PartInfo -> (Either PolicyViolationException FilePath) -> IO (AccValidation Text (Text, Text))
    handleRead partInfo = either lefty righty
      where
        key    = partInfo |> (partFileName >>> (fromMaybe "-") >>> decodeUtf8)
        lefty  = policyViolationExceptionReason  >>> (_Failure #) >>> return
        righty = readPossibleGZip >>> liftIO >=> ((key,) >>> (_Success #) >>> return)
          where
            readPossibleGZip filepath = catch (readGZip filepath) (\e -> const (readFile filepath) (e :: DecompressError))
              where
                readGZip = (LazyByteString.readFile >=> (decompress >>> LazyTextEncoding.decodeUtf8 >>> LazyText.toStrict >>> return'))

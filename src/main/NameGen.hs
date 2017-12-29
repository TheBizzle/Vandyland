{-# LANGUAGE TemplateHaskell #-}
module NameGen(generateName) where

import Bizzlelude

import Data.ByteString(ByteString)
import Data.ByteString.Char8(split)
import Data.FileEmbed(embedFile)
import Data.List((!!))
import Data.Text.Encoding(decodeUtf8)

import System.Random(randomRIO)

generateName :: IO Text
generateName = (\x y -> x <> " " <> y) <$> (randomOneOf adjectives) <*> (randomOneOf animals)

randomOneOf :: [a] -> IO a
randomOneOf xs =
  do
    index <- randomRIO (0, (length xs) - 1)
    return $ xs !! index

adjectives :: [Text]
adjectives = format $(embedFile "adjectives.txt")

animals :: [Text]
animals = format $(embedFile "animals.txt")

format :: ByteString -> [Text]
format = (split '\n') >>> (map decodeUtf8)

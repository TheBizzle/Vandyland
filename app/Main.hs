module Main(main) where

import Control.Applicative((<|>))

import Snap.Core(dir, route)
import Snap.Http.Server(quickHttpServe)
import Snap.Util.FileServe(serveDirectory)

import qualified Vandyland.Gallery.Controller as Gallery

main :: IO ()
main = quickHttpServe $ route (Gallery.routes) <|> dir "html" (serveDirectory "html")

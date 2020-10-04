module Main(main) where

import Snap.Core(dir, route)
import Snap.Http.Server(quickHttpServe)
import Snap.Util.FileServe(serveDirectory)

import qualified Vandyland.BadgerState.Controller as BadgerState
import qualified Vandyland.Gallery.Controller     as Gallery

main :: IO ()
main = quickHttpServe $ route (BadgerState.routes <> Gallery.routes) <|> dir "html" (serveDirectory "html")

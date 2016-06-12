{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Monoid ((<>))

import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.String as String
import qualified Data.Version as Version
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Network.Wai.Parse as Parse
import qualified Octane
import qualified Paths_octane_server as This
import qualified System.Environment as Environment
import qualified Text.Read as Read


main :: IO ()
main = do
    settings <- getSettings
    Warp.runSettings settings applicationWithMiddleware


getSettings :: IO Warp.Settings
getSettings = do
    maybeHost <- Environment.lookupEnv "HOST"
    let host = case maybeHost of
            Nothing -> "127.0.0.1"
            Just x -> String.fromString x

    maybePort <- Environment.lookupEnv "PORT"
    let port = case maybePort & fmap Read.readMaybe & Monad.join of
            Nothing -> 8080
            Just x -> x

    let serverName = "octane-server-" <> version

    let settings = Warp.defaultSettings
            & Warp.setHost host
            & Warp.setPort port
            & Warp.setServerName serverName
    pure settings


version :: (String.IsString string) => string
version = This.version & Version.showVersion & String.fromString


applicationWithMiddleware :: Wai.Application
applicationWithMiddleware = middleware application


middleware :: Wai.Middleware
middleware
    = RequestLogger.logStdout
    >>> Gzip.gzip Gzip.def


application :: Wai.Application
application request respond = do
    let path = Wai.pathInfo request
    let method = Wai.requestMethod request

    response <- case path of
        [] -> case method of
            "GET" -> getRoot
            _ -> notAllowed
        ["api", "v1", "replays"] -> case method of
            "POST" -> postReplays request
            _ -> notAllowed
        _ -> notFound

    respond response


getRoot :: IO Wai.Response
getRoot = do
    let status = Http.ok200
    let headers = [(Http.hContentType, "text/html")]
    let body = "<!doctype html>\
        \<html>\
            \<head>\
                \<meta charset='utf-8'>\
                \<title>Octane</title>\
                \<style>\
                    \body {\
                        \font-family:sans-serif;\
                        \text-align:center;\
                    \}\
                \</style>\
            \</head>\
            \<body>\
                \<h1>Octane</h1>\
                \<p>\
                    \Upload a Rocket League replay. Get some JSON.\
                \</p>\
                \<form action='api/v1/replays' enctype='multipart/form-data' method='post'>\
                    \<input name='replay' type='file'>\
                    \<input type='submit'>\
                \</form>\
                \<p>\
                    \<a href='https://github.com/tfausak/octane'>\
                        \Octane " <> octaneVersion <> "\
                    \</a>\
                \</p>\
            \</body>\
        \</html>"
    let response = Wai.responseLBS status headers body
    pure response


octaneVersion :: (String.IsString string) => string
octaneVersion = Octane.version & Version.showVersion & String.fromString


postReplays :: Wai.Request -> IO Wai.Response
postReplays request = do
    (_, files) <- Parse.parseRequestBody Parse.lbsBackEnd request

    case lookup "replay" files of
        Nothing -> badRequest
        Just file -> do
            let content = Parse.fileContent file
            let replay = Octane.unsafeParseReplay content

            let status = Http.ok200
            let headers = [(Http.hContentType, "application/json")]
            let body = Aeson.encode replay
            let response = Wai.responseLBS status headers body
            pure response


badRequest :: IO Wai.Response
badRequest = do
    let response = emptyResponse Http.badRequest400
    pure response


notFound :: IO Wai.Response
notFound = do
    let response = emptyResponse Http.notFound404
    pure response


notAllowed :: IO Wai.Response
notAllowed = do
    let response = emptyResponse Http.methodNotAllowed405
    pure response


emptyResponse :: Http.Status -> Wai.Response
emptyResponse status = Wai.responseLBS status [] ""

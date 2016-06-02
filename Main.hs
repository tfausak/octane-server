{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Monoid ((<>))

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.String as String
import qualified Data.Version as Version
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Network.Wai.Parse as Parse
import qualified Octane


main :: IO ()
main = Warp.runEnv 8080 (middleware application)


middleware :: Wai.Middleware
middleware
    = RequestLogger.logStdout
    . Gzip.gzip Gzip.def


application :: Wai.Application
application request respond = do
    let path = Wai.pathInfo request
    let method = Wai.requestMethod request

    response <- case path of
        [] -> case method of
            "GET" -> getRoot
            _ -> notAllowed
        ["replays"] -> case method of
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
                        \text-align: center;\
                    \}\
                \</style>\
            \</head>\
            \<body>\
                \<h1>Octane</h1>\
                \<p>\
                    \Upload a Rocket League replay. Get some JSON.\
                \</p>\
                \<form action='replays' enctype='multipart/form-data' method='post'>\
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
    pure (Wai.responseLBS status headers body)


octaneVersion :: (String.IsString string) => string
octaneVersion = String.fromString (Version.showVersion Octane.version)


postReplays :: Wai.Request -> IO Wai.Response
postReplays request = do
    (_, files) <- Parse.parseRequestBody Parse.lbsBackEnd request

    case lookup "replay" files of
        Nothing -> badRequest
        Just file -> do
            let content = Parse.fileContent file
            let replay = Binary.decode content
            let frames = Octane.parseFrames replay

            let status = Http.ok200
            let headers = [(Http.hContentType, "application/json")]
            let body = encodeReplay replay frames
            pure (Wai.responseLBS status headers body)


encodeReplay :: Octane.Replay -> [Octane.Frame] -> ByteString.ByteString
encodeReplay replay frames = Aeson.encode (Aeson.object
    [ ("frames", Aeson.toJSON frames)
    , ("meta", Aeson.toJSON replay)
    ])


badRequest :: IO Wai.Response
badRequest = pure (Wai.responseLBS Http.badRequest400 [] "")


notFound :: IO Wai.Response
notFound = pure (Wai.responseLBS Http.notFound404 [] "")


notAllowed :: IO Wai.Response
notAllowed = pure (Wai.responseLBS Http.methodNotAllowed405 [] "")

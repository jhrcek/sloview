{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>))
import Snap.Core (dir, ifTop, route, Snap)
import Snap.Http.Server (simpleHttpServe)
import Snap.Http.Server.Config (defaultConfig, Config, ConfigLog(ConfigNoLog), setAccessLog)
import Snap.Util.FileServe (serveDirectory)

import Handler.Index as Index
import Handler.Upload as Upload

main :: IO ()
main = simpleHttpServe config site

config :: Config Snap a
config = setAccessLog ConfigNoLog defaultConfig

site :: Snap ()
site =
    ifTop (Index.indexHandler [])
    <|> route [("/doUpload", Upload.uploadHandler)]
    <|> dir "static" (serveDirectory "static")

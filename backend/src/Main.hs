{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>))
import Snap.Core (dir, ifTop, route, Snap)
import Snap.Http.Server (simpleHttpServe)
import Snap.Http.Server.Config (defaultConfig, Config, ConfigLog(ConfigNoLog, ConfigIoLog), setAccessLog, setErrorLog)
import Snap.Util.FileServe (serveDirectory)

import Handler.Index as Index
import Handler.Upload as Upload
import Model.ServerLog as SL

main :: IO ()
main = simpleHttpServe config site

config :: Config Snap a
config =
  setAccessLog ConfigNoLog $
  setErrorLog (ConfigIoLog print)
  defaultConfig

site :: Snap ()
site =
    ifTop (Index.indexHandler SL.initialServerLog)
    <|> route [("/doUpload", Upload.uploadHandler)]
    <|> dir "static" (serveDirectory "static")

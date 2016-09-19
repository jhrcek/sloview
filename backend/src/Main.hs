{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>))
import Snap.Core (sendFile, dir, ifTop, Snap)
import Snap.Util.FileServe (serveDirectory)
import Snap.Http.Server (simpleHttpServe)
import Snap.Http.Server.Config (defaultConfig, Config, ConfigLog(ConfigNoLog), setAccessLog, setErrorLog)

main :: IO ()
main = simpleHttpServe config site

config :: Config Snap a
config =
  setAccessLog ConfigNoLog $
  setErrorLog ConfigNoLog $
  defaultConfig

site :: Snap ()
site =
    ifTop (sendFile "index.html") <|>
    dir "static" (serveDirectory "static")

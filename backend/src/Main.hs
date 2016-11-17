{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>))
import Snap.Core (dir, ifTop, Snap, writeLazyText)
import Snap.Util.FileServe (serveDirectory)
import Snap.Http.Server (simpleHttpServe)
import Snap.Http.Server.Config (defaultConfig, Config, ConfigLog(ConfigNoLog), setAccessLog, setErrorLog)
import qualified Text.Blaze.Html.Renderer.Text as Blaze

import Pages.Index as Index


main :: IO ()
main = simpleHttpServe config site

config :: Config Snap a
config =
  setAccessLog ConfigNoLog $
  setErrorLog ConfigNoLog $
  defaultConfig

site :: Snap ()
site =
  ifTop indexHandler <|>
  dir "static" (serveDirectory "static")

indexHandler :: Snap ()
indexHandler = writeLazyText . Blaze.renderHtml $ Index.generate "Elm.Main.fullscreen()"

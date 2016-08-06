{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>))
import Snap.Core (sendFile, dir, ifTop, Snap)
import Snap.Util.FileServe (serveDirectory)
import Snap.Http.Server (quickHttpServe)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (sendFile "index.html") <|>
    dir "static" (serveDirectory "static")

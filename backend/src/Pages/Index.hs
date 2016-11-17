{-# LANGUAGE OverloadedStrings #-}
module Pages.Index (generate) where

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

generate :: String -> H.Html
generate elmInitCode =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.title $ H.toHtml ("server.log viewer" :: String)
      H.link ! A.rel "stylesheet" ! A.href "static/css/style.css"
      H.script ! A.src "static/js/app.js" $ ""
    H.body $ do
      H.script $ H.preEscapedToMarkup elmInitCode

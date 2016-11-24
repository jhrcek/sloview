{-# LANGUAGE OverloadedStrings #-}
module Handler.Index (handler) where

import Snap.Core (Snap, writeLazyText)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

handler :: Snap ()
handler = writeLazyText . renderHtml $ generate "Elm.Main.fullscreen()"

generate :: String -> H.Html
generate elmInitCode =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.title $ H.toHtml ("server.log viewer" :: String)
      H.link ! A.rel "stylesheet" ! A.href "static/css/style.css"
      H.script ! A.src "static/js/app.js" $ ""
    H.body $
      H.script $ H.preEscapedToMarkup elmInitCode

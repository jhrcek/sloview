{-# LANGUAGE OverloadedStrings #-}
module Handler.Index (indexHandler) where

import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Snap.Core (Snap, writeLazyText)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Model.ServerLog

indexHandler :: ServerLog -> Snap ()
indexHandler sl =
  let markup = renderHtml $ generatePage sl
  in  writeLazyText markup

generatePage :: ServerLog -> H.Html
generatePage sl =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "UTF-8"
      H.title $ H.toHtml ("server.log viewer" :: String)
      H.link ! A.rel "stylesheet" ! A.href "static/css/style.css"
      H.script ! A.src "static/js/app.js" $ ""
    H.body $
      H.script $ H.unsafeLazyByteString $ generateElmInitCode sl


generateElmInitCode :: ServerLog -> ByteString
generateElmInitCode sl = BS.concat $ map BS.pack
    [ "Elm.Main.fullscreen("
    , show $ encode sl --TODO using show as a hack to escape '"' in the json - how to do it better?
                       -- i.e. how to convert lazy json in ByteString to be rendered on page?
    , ")"
    ]

{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings -}
module Blog.Views where

import Web.Request
import Web.Response
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Web.Utils
import Text.XHtml

mainIndexPage = body << h1 << "Hello, from Luke's web framework"

mainIndex :: Request -> IO (Maybe Response)
mainIndex req = let resp = buildResponse utf8HtmlResponse [
                            addContent $ BS.pack $ renderHtml mainIndexPage
                           ]
                in return $ Just resp

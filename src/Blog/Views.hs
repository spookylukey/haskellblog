{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Blog.Views where

import Web.Request
import Web.Response
import Web.Utils
import Blog.Templates

mainIndex :: Request -> IO (Maybe Response)
mainIndex req = let resp = buildResponse utf8HtmlResponse [
                            addHtml mainIndexPage
                           ]
                in return $ Just resp

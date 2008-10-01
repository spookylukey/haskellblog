{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Blog.Views where

import Web.Request
import Web.Response
import Web.Utils
import Blog.Templates

mainIndex :: Request -> IO (Maybe Response)
mainIndex req = return $ Just $ buildResponse utf8HtmlResponse [
                 addHtml mainIndexPage
                ]

debug path req = return $ Just $ buildResponse utf8TextResponse [
                  addContent "Path:\n"
                 , addContent $ utf8 path
                 , addContent "\n\nRequest:\n"
                 , addContent $ utf8 $ show req
                 ]

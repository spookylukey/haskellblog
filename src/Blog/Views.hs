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

debug path req = return $ Just $ buildResponse utf8TextResponse [
                  addContent "Path:\n"
                 , addContent $ utf8 path
                 , addContent "\n\nRequest:\n"
                 , addContent $ utf8 $ show req
                 ]

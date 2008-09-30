{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Blog.Views where

import Web.Request
import Web.Response
import Data.ByteString.Lazy.Char8 (ByteString)
import Web.Utils
import Web.Framework (View)

mainIndexPage :: ByteString
mainIndexPage = "<h1>Hello, from “Luke's é web framework”</h1>" 

mainIndex :: View
mainIndex req = let resp = buildResponse utf8HtmlResponse [
                            addContent mainIndexPage
                           ]
                in return $ Just resp

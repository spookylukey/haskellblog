{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Web.Response

where

import Web.Response
import Test.HUnit
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

testAddContent1 = "“Hello”" ~=? (content $ addContent "“Hello”" $ emptyResponse)

testAddContent2 = "Hello world" ~=? (content $ addContent " world" $ addContent "Hello" $ emptyResponse)

testBuildResponse = "hello world" ~=? (content $
                                       buildResponse [ addContent "hello"
                                                     , addContent " world"
                                                     ] utf8HtmlResponse)

testFormatResponse = "Content-type: text/html; charset=UTF-8\r\n\
                     \Status: 200\r\n\
                     \\r\n\
                     \<h1>Test</h1>" ~=?
                     (formatResponse $ buildResponse [
                                          addContent "<h1>Test</h1>"
                                         ] utf8HtmlResponse)

testFormatResponse2 = "Content-type: text/html; charset=UTF-8\r\n\
                      \Status: 404\r\n\
                      \\r\n\
                      \<h1>404 Not Found</h1>" ~=?
                      (formatResponse $ buildResponse [
                                           addContent "<h1>404 Not Found</h1>"
                                          , setStatus 404
                                          ] utf8HtmlResponse)

tests = test [
          testAddContent1
        , testAddContent2
        , testBuildResponse
        , testFormatResponse
        , testFormatResponse2
        ]

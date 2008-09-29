{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Web.Response

where

import Web.Response
import Test.HUnit
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import GHC.Exts( IsString(..) )
instance IsString ByteString where
    fromString = BS.pack

testAddContent1 = "Hello" ~=? (content $ addContent "Hello" $ emptyResponse)

testAddContent2 = "Hello world" ~=? (content $ addContent " world" $ addContent "Hello" $ emptyResponse)

testBuildResponse = "hello world" ~=? (content $
                                       buildResponse utf8HtmlResponse [
                                                          addContent "hello",
                                                          addContent " world"
                                                         ])

testFormatResponse = "Content-type: text/html; charset=UTF-8\r\n\
                     \Status: 200\r\n\
                     \\r\n\
                     \<h1>Test</h1>" ~=? (formatResponse $
                                          buildResponse utf8HtmlResponse [
                                                             addContent "<h1>Test</h1>"
                                                            ])

tests = test [
          testAddContent1
        , testAddContent2
        , testBuildResponse
        , testFormatResponse
        ]

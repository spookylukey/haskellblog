module Tests.Web.Response

where

import Web.Response
import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as BS

testAddContent1 = "Hello" ~=? (BS.unpack $ content $ addContent (BS.pack "Hello") $ emptyResponse)

testAddContent2 = "Hello world" ~=? (BS.unpack $ content $ addContent (BS.pack " world") $ addContent (BS.pack "Hello") $ emptyResponse)

testBuildResponse = "hello world" ~=? (BS.unpack $ content $
                                         buildResponse utf8HtmlResponse [
                                                            addContent (BS.pack "hello"),
                                                            addContent (BS.pack " world")
                                                           ])

tests = test [
          testAddContent1
        , testAddContent2
        , testBuildResponse
        ]

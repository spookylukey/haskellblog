{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Web.Request

where

import Web.Request
import Test.HUnit
import Web.GenUtils () -- for IsString instance

testMethod = "GET" ~=? requestMethod (mkRequest [("REQUEST_METHOD","GET")] "" utf8Encoding)
testPath = "foo/bar" ~=? pathInfo (mkRequest [("PATH_INFO", "/foo/bar")] "" utf8Encoding)
testPathMissing = "" ~=? pathInfo (mkRequest [] "" utf8Encoding)
testPathUtf8 = "\233" ~=? pathInfo (mkRequest [("PATH_INFO", "\195\169")] "" utf8Encoding)

tests = test [
          testMethod
        , testPath
        , testPathMissing
        , testPathUtf8
        ]

{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Web.Request

where

import Web.Request
import Test.HUnit
import Web.Utils

testMethod = "GET" ~=? requestMethod (mkRequest [("REQUEST_METHOD","GET")] "")
testPath = "/foo/bar" ~=? pathInfo (mkRequest [("PATH_INFO", "/foo/bar")] "")

tests = test [
         testMethod,
         testPath
        ]

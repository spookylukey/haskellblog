module Tests.Web.Request

where

import Web.Request
import Test.HUnit

testMethod = "GET" ~=? requestMethod (mkRequest [("REQUEST_METHOD","GET")] "")
testPath = "/foo/bar" ~=? pathInfo (mkRequest [("PATH_INFO", "/foo/bar")] "")

tests = test [
         testMethod,
         testPath
        ]

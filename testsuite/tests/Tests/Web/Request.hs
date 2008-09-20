module Tests.Web.Request

where

import qualified Web.Request as R
import Web.Request (mkRequest)
import Test.HUnit

testMethod = "GET" ~=? R.method (mkRequest [("REQUEST_METHOD","GET")] "")
testPath = "/foo/bar" ~=? R.path (mkRequest [("PATH_INFO", "/foo/bar")] "")

tests = test [
         testMethod,
         testPath
        ]

module Tests.Web.Request

where

import qualified Web.Request as R
import Test.HUnit

testMethod = "GET" ~=? R.method (R.mkRequest [("REQUEST_METHOD","GET")] "")
testPath = "/foo/bar" ~=? R.path (R.mkRequest [("PATH_INFO", "/foo/bar")] "")

tests = test [
         testMethod,
         testPath
        ]

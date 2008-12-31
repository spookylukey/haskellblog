module Tests.Blog.Views where

import Blog.Settings
import Ella.TestUtils
import Test.HUnit

login_req1 = mkPostReq "/login/" [("username", "luke")
                                 ,("password", "testpassword")
                                 ]

tests = test [ ]

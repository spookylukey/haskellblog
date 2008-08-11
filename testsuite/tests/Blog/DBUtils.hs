module Tests.Blog.DBUtils

where

import Blog.DBUtils
import Test.HUnit

tests = test [
         "slugFromTitle1" ~: "this-is-a-title" ~=? (slugFromTitle "This is a % $ /title ^£$")
        ]

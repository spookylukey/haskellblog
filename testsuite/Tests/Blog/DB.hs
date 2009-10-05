module Tests.Blog.DB

where

import Blog.DB
import Test.HUnit

testMkUpdateStatement = "UPDATE foo SET bar=?, baz=?" ~=?
                        mkUpdateStatement "foo" ["bar", "baz"]

tests = test [ testMkUpdateStatement
             ]

module Tests.Blog.Views where

import Blog.Model
import Blog.Settings
import Blog.Views
import Ella.Response
import Ella.TestUtils
import Test.HUnit
import Tests.Blog.TestDB (connect)
import Tests.Blog.Model (withEmptyUsersTable)

login_req_bad = mkPostReq "/login/" [("username", "testusername")
                                    ,("password", "testpassword2")
                                    ]
login_req_good = mkPostReq "/login/" [("username", "testusername")
                                     ,("password", "testpassword")
                                     ]


withOneUser action = withEmptyUsersTable (\cn ->
                                              do
                                                createUser cn "testusername" False
                                                setPassword cn "testusername" "testpassword"
                                                action cn)

testLoginFails = withOneUser
                 (\cn -> do
                    Just resp1 <- loginView' cn login_req_bad
                    assertEqual "No cookies should be set" 0 (length $ cookies resp1)
                 )


testLoginSucceeds = withOneUser
                    (\cn -> do
                       Just resp1 <- loginView' cn login_req_good
                       assertEqual "Username cookies should be set" 1 (length $ cookies resp1)
                    )

tests = test [ "login fails" ~: testLoginFails
             , "login succeeds" ~: testLoginSucceeds
             ]

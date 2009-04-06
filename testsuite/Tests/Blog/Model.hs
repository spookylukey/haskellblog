module Tests.Blog.Model where

import Blog.Model
import Database.HDBC
import Control.Exception (bracket)
import Test.HUnit
import qualified Blog.DB as DB
import qualified Tests.Blog.TestDB as TestDB


withEmptyUsersTable = bracket (do
                                cn <- TestDB.connect
                                run cn "DELETE FROM users;" []
                                commit cn
                                return cn)
                               (\cn -> do
                                    run cn "DELETE FROM users;" []
                                    commit cn
                                    disconnect cn
                                    return ())

testSetPassword = withEmptyUsersTable
                  (\cn -> do
                     res1 <- checkPassword cn "testuser" "testpassword"
                     assertBool "Password check should fail with no user." (not res1)

                     createUser cn "testuser" True

                     setPassword cn "testuser" "testpassword"
                     res3 <- checkPassword cn "testuser" "testpassword"
                     assertBool "Password check should succeed" res3

                     res4 <- checkPassword cn "testuser" "somethingelse"
                     assertBool "Password check should fail with wrong password" (not res4)

                  )

tests = test [ "set password" ~: testSetPassword
             ]

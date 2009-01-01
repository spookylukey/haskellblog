module Tests.Blog.Model where

import Blog.Model
import Database.HDBC
import Control.Exception (bracket)
import Test.HUnit
import qualified Blog.DB as DB
import qualified Tests.Blog.TestDB as TestDB


withEmptyUsersTable = bracket (do
                                cn <- TestDB.connect
                                _ <- quickQuery' cn "DELETE FROM users;" []
                                commit cn
                                return cn)
                               (\cn -> do
                                    _ <- quickQuery' cn "DELETE FROM users;" []
                                    commit cn
                                    return ())

testSetPassword = withEmptyUsersTable
                  (\cn -> do
                     res1 <- checkPassword cn "testuser" "testpassword"
                     assertBool "Password check should fail with no user." (not res1)

                     DB.doInsert cn "users" [ "username", "password"] [ toSql "testuser", toSql "sha1:foo:bar" ]

                     setPassword cn "testuser" "testpassword"
                     res3 <- checkPassword cn "testuser" "testpassword"
                     assertBool "Password check should succeed" res3

                     res4 <- checkPassword cn "testuser" "somethingelse"
                     assertBool "Password check should fail with wrong password" (not res4)

                  )

tests = test [ "set password" ~: testSetPassword
             ]

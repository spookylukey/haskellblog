{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Tests.Web.Framework.Processors

where

import Data.Maybe (fromJust)
import Web.Framework.Processors
import Web.GenUtils ()
import Web.Response
import Web.Request
import Test.HUnit
import Tests.Web.Framework -- reuse view functions

testAddSlashRedirectProcessor1 =
    (do
      resp <- addSlashRedirectProcessor alwaysSucceedView1 (mkGetReq "/posts")
      return (resp == (Just $ redirectResponse "/posts/"))
    ) ~? "addSlashRedirectProcessor should add a slash if not present at end"


testAddSlashRedirectProcessor2 =
    (do
      resp <- addSlashRedirectProcessor alwaysSucceedView1 (mkGetReq "/posts/")
      return (resp == (Just resp1))
    ) ~? "addSlashRedirectProcessor should not redirect if slash present at end"

testAddSlashRedirectProcessor3 =
    (do
      resp <- addSlashRedirectProcessor alwaysSucceedView1 (mkRequest
                                                            [("REQUEST_METHOD", "GET")
                                                            ,("PATH_INFO", "/posts")
                                                            ,("REQUEST_URI","/foo/posts")]
                                                            "" utf8Encoding)
      return (resp == (Just $ redirectResponse "/foo/posts/"))
    ) ~? "addSlashRedirectProcessor should redirect based on request URI, not path info"



tests = test [ testAddSlashRedirectProcessor1
             , testAddSlashRedirectProcessor2
             , testAddSlashRedirectProcessor3
             ]

module Tests.Web.Framework.Processors

where

import Data.Maybe (fromJust)
import Web.Framework.Processors
import Web.Response
import Test.HUnit
import Tests.Web.Framework -- reuse view functions

testAddSlashRedirectProcessor1 =
    (do
      resp <- addSlashRedirectProcessor alwaysSucceedView1 (mkGetReq "posts")
      return (resp == (Just $ redirectResponse "posts/"))
    ) ~? "addSlashRedirectProcessor should add a slash if not present at end"


testAddSlashRedirectProcessor2 =
    (do
      resp <- addSlashRedirectProcessor alwaysSucceedView1 (mkGetReq "/posts/")
      return (resp == (Just resp1))
    ) ~? "addSlashRedirectProcessor should not redirect if slash present at end"


tests = test [ testAddSlashRedirectProcessor1
             , testAddSlashRedirectProcessor2
             ]

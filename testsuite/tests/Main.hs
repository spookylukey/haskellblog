import qualified Tests.Blog.DBUtils as DBUtils
import qualified Tests.Web.Request as Request
import qualified Tests.Web.Response as Response
import Test.HUnit

main = runTestTT (test [
                    DBUtils.tests
                  , Request.tests
                  , Response.tests
                  ])

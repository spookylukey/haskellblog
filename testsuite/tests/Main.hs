import qualified Tests.Blog.DBUtils as DBUtils
import qualified Tests.Web.Request as Request
import Test.HUnit

main = runTestTT (test [
                   DBUtils.tests,
                   Request.tests
                  ])

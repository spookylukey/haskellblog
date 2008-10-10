import qualified Tests.Blog.DBUtils as DBUtils
import qualified Tests.Web.Request as Request
import qualified Tests.Web.Response as Response
import qualified Tests.Web.Framework as Framework
import qualified Tests.Web.Processors.General as GeneralProcessors
import Test.HUnit

main = runTestTT (test [
                    DBUtils.tests
                  , Request.tests
                  , Response.tests
                  , Framework.tests
                  , GeneralProcessors.tests
                  ])

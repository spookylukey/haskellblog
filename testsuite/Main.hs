import qualified Tests.Blog.DBUtils as DBUtils
import qualified Tests.Blog.Formats as Formats
import Test.HUnit

main = runTestTT (test [ "DBUtils tests" ~: DBUtils.tests
                       , "Format tests" ~: Formats.tests
                       ])

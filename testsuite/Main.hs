import qualified Tests.Blog.DBUtils as DBUtils
import qualified Tests.Blog.Formats as Formats
import qualified Tests.Blog.Model as Model
import Test.HUnit

main = runTestTT (test [ "DBUtils tests" ~: DBUtils.tests
                       , "Format tests" ~: Formats.tests
                       , "Model tests" ~: Model.tests
                       ])

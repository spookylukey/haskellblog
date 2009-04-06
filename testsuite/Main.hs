import qualified Tests.Blog.DBUtils as DBUtils
import qualified Tests.Blog.Formats as Formats
import qualified Tests.Blog.Model as Model
import qualified Tests.Blog.Views as View
import Test.HUnit

main = runTestTT (test [ "DBUtils tests" ~: DBUtils.tests
                       , "Format tests" ~: Formats.tests
                       , "Model tests" ~: Model.tests
                       , "View tests" ~: View.tests
                       ])

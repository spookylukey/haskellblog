import qualified Tests.Blog.DBUtils as DBUtils
import Test.HUnit

main = runTestTT (test [
                    DBUtils.tests
                  ])

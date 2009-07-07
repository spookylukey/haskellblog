import Blog.Routes (views)
import Blog.Views (custom404handler)
import Database.HDBC
import Ella.Framework
import Ella.Processors.Security (signedCookiesProcessor)
import qualified Blog.Settings as Settings

options = defaultDispatchOptions { notFoundHandler = custom404handler
                                 , viewProcessors = [signedCookiesProcessor Settings.secret]
                                 }

sqlDebug action = catchSql (do { action; return ()}) (\e -> sendResponseCGI $ default500 $ show e)

main :: IO ()
main = sqlDebug $ dispatchCGI views options

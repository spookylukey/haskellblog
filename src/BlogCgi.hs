import Blog.Routes (views)
import Blog.Views (custom404)
import Database.HDBC
import Ella.Framework
import Ella.Processors.General (signedCookiesProcessor)
import qualified Blog.Settings as Settings

options = defaultDispatchOptions { notFoundHandler = const $ return $ custom404
                                 , viewProcessors = [signedCookiesProcessor Settings.secret]
                                 }

sqlDebug action = catchSql (do { action; return ()}) (\e -> sendResponseCGI $ default500 $ show e)

main :: IO ()
main = sqlDebug $ dispatchCGI views options

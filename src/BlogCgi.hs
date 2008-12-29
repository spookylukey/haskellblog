import Ella.Framework
import Blog.Routes (views)
import Blog.Views (custom404)

import Database.HDBC

options = defaultDispatchOptions { notFoundHandler = const $ return $ custom404
                                 }

sqlDebug action = catchSql (do { action; return ()}) (\e -> sendResponseCGI $ default500 $ show e)

main :: IO ()
main = sqlDebug $ dispatchCGI views options

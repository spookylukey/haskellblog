import Blog.Routes (views)
import Blog.Views (return404)
import Blog.Globals (csrfProtectionProcessor)
import Database.HDBC
import Ella.Framework
import Ella.Processors.Security (signedCookiesProcessor)
import qualified Blog.Settings as Settings

options = defaultDispatchOptions { notFoundHandler = return404
                                 , viewProcessors = [ signedCookiesProcessor Settings.secret
                                                    , csrfProtectionProcessor
                                                    ]
                                 }

sqlDebug action = catchSql (do { action; return ()}) (\e -> sendResponseCGI $ default500 $ show e)

main :: IO ()
main = sqlDebug $ dispatchCGI views options

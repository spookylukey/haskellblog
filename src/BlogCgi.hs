import Blog.Routes (views)
import Ella.Framework
import Blog.Views (custom404)

options = defaultDispatchOptions { notFoundHandler = const $ return $ custom404
                                 }

main :: IO ()
main = dispatchCGI views options

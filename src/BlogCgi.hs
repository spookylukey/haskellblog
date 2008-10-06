import Blog.Routes
import Web.Framework

main :: IO ()
main = dispatchCGI routes defaultDispatchOptions

import Blog.Views
import Web.Framework

views = [
          empty //-> mainIndex 
        ]

main :: IO ()
main = dispatchCGI views defaultDispatchOptions

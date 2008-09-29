import Blog.Views
import Web.Framework

views = [ 
          mainIndex 
        ]

main :: IO ()
main = dispatchCGI views defaultDispatchOptions

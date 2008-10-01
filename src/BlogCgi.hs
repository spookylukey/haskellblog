import Blog.Views
import Web.Framework

views = [ empty                       //-> mainIndex
        , "debug/" <+/> stringParam   //-> debug
        ]

main :: IO ()
main = dispatchCGI views defaultDispatchOptions

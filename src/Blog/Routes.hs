module Blog.Routes where

import Blog.Views
import Blog.Processors
import Ella.Framework
import Ella.Processors.General (addSlashRedirectView)
import Ella.GenUtils (apply)

-- * Routes

-- These need to be manually synced with Blog.Links

views  = [ addSlashRedirectView
         , canonicalUri
         , empty                                      //-> mainIndex              $ []
         , "posts/" <+/> anyParam                     //-> postView               $ []
         , "posts/" <+/> empty                        //-> postsRedirectView      $ []
         , "categories/" <+/> empty                   //-> categoriesView         $ []
         , "categories/" <+/> anyParam                //-> categoryView           $ []
         , "about/" <+/> empty                        //-> aboutView              $ []
         , "feeds/" <+/> empty                        //-> feedsView              $ []
         , "debug/" <+/> anyParam                     //-> debug                  $ []
         ]

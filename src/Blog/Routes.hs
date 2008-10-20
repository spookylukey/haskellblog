module Blog.Routes where

import Blog.Views
import Blog.Processors
import Web.Framework
import Web.Processors.General (addSlashRedirectView)
import Web.GenUtils (apply)

-- * Routes

-- These need to be manually synced with Blog.Links

views  = [ addSlashRedirectView
         , canonicalUri
         , empty                                      //-> mainIndex              $ []
         , "posts/" <+/> stringParam                  //-> postView               $ []
         , "posts/" <+/> empty                        //-> postsRedirectView      $ []
         , "categories/" <+/> empty                   //-> categoriesView         $ []
         , "categories/" <+/> stringParam             //-> categoryView           $ []
         , "debug/" <+/> stringParam                  //-> debug                  $ []
         ]

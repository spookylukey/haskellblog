module Blog.Routes where

import Web.Framework
import Blog.Views

-- * Routes

-- These need to be manually synced with Blog.Links

routes = [ empty                                      //-> mainIndex
         , "posts/" <+/> stringParam                  //-> postView
         , "posts/" <+/> empty                        //-> postsRedirectView
         , "categories/" <+/> empty                   //-> categoriesView
         , "categories/" <+/> stringParam             //-> categoryView
         , "debug/" <+/> stringParam                  //-> debug
         ]

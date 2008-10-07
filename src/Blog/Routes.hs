module Blog.Routes where

import Web.Framework
import Blog.Views

-- * Routes

-- These need to be manually synced with Blog.Links

indexRoute         = empty
postRoute          = "posts/" <+/> stringParam
postsRedirect      = "posts/" <+/> empty
categoriesRoute    = "categories/" <+/> empty
categoryRoute      = "categories/" <+/> stringParam
debugRoute         = "debug/" <+/> stringParam

routes = [ indexRoute        //-> mainIndex
         , postRoute         //-> postView
         , postsRedirect     //-> postsRedirectView
         , categoriesRoute   //-> categoriesView
         , categoryRoute     //-> categoryView
         , debugRoute        //-> debug
         ]

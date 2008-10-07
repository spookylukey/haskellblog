module Blog.Routes where

import Web.Framework
import Blog.Views

-- * Routes

-- These need to be manually synced with Blog.Links

indexRoute         = empty
postRoute          = "posts/" <+/> stringParam
categoriesRoute    = "categories/" <+/> empty
categoryRoute      = "categories/" <+/> stringParam
debugRoute         = "debug/" <+/> stringParam

routes = [ indexRoute        //-> mainIndex
         , postRoute         //-> postView
         , categoriesRoute   //-> categoriesView
         , categoryRoute     //-> categoryView
         , debugRoute        //-> debug
         ]

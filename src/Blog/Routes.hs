module Blog.Routes where

import Blog.Views
import Web.Framework
import Web.Framework.Processors (addSlashRedirectProcessor)
import Web.GenUtils (apply)

-- * Routes

-- These need to be manually synced with Blog.Links

views' = [ empty                                      //-> mainIndex
         , "posts/" <+/> stringParam                  //-> postView
         , "posts/" <+/> empty                        //-> postsRedirectView
         , "categories/" <+/> empty                   //-> categoriesView
         , "categories/" <+/> stringParam             //-> categoryView
         , "debug/" <+/> stringParam                  //-> debug
         ]

-- Apply global processors to all views
procs = [addSlashRedirectProcessor]

views = map (apply procs) views'

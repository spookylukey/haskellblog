module Blog.Routes where

import Blog.Views
import Blog.Processors
import Web.Framework
import Web.Framework.Processors (addSlashRedirectProcessor)
import Web.GenUtils (apply)

-- * Routes

-- These need to be manually synced with Blog.Links

views' = [ empty                                      //-> mainIndex              $ []
         , "posts/" <+/> stringParam                  //-> postView               $ []
         , "posts/" <+/> empty                        //-> postsRedirectView      $ []
         , "categories/" <+/> empty                   //-> categoriesView         $ []
         , "categories/" <+/> stringParam             //-> categoryView           $ []
         , "debug/" <+/> stringParam                  //-> debug                  $ []
         ]

-- Apply global processors to all views.  NB - these processors run
-- even if the matcher will not succeed, so this should only be done
-- for processors which either require this behaviour, or are low
-- enough overhead to be done anyway.
procs = [ addSlashRedirectProcessor
        , canonicalUri
        ]

views = map (apply procs) views'

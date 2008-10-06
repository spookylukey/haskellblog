module Blog.Routes where

import qualified Blog.Category as C
import qualified Blog.Post as P
import qualified Blog.Settings as Settings
import Web.Framework
import Blog.Views

-- TODO - a better way of generating this, something like Routes

-- * Routes

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


-- * URL functions

-- These provide handy wrappers for generating URLs from objects

makePostUrl p = Settings.root_url ++ "posts/" ++ (P.slug p) ++ "/"
makeCategoryUrl c = Settings.root_url ++ "categories/" ++ (C.slug c) ++ "/"


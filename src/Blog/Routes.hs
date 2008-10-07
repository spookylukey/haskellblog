module Blog.Routes where

import qualified Blog.Category as C
import qualified Blog.Post as P
import qualified Blog.Settings as Settings
import Web.Framework
import Blog.Views

-- * Routes

-- We have to manually ensure that the permalink functions match the
-- routes specified for the views.

indexRoute         = empty
indexLink          = Settings.root_url

postRoute          = "posts/" <+/> stringParam
postLink p         = Settings.root_url ++ "posts/" ++ (P.slug p) ++ "/"

categoriesRoute    = "categories/" <+/> empty
categoriesLink     = Settings.root_url ++ "categories/"

categoryRoute      = "categories/" <+/> stringParam
categoryLink c     = Settings.root_url ++ "categories/" ++ (C.slug c) ++ "/"

debugRoute         = "debug/" <+/> stringParam

routes = [ indexRoute        //-> mainIndex
         , postRoute         //-> postView
         , categoriesRoute   //-> categoriesView
         , categoryRoute     //-> categoryView
         , debugRoute        //-> debug
         ]

module Blog.Routes where

import qualified Blog.Category as C
import qualified Blog.Post as P
import qualified Blog.Settings as Settings
import Web.Framework
import Blog.Views

-- TODO - a better way of generating this, something like Routes

makePostUrl p = Settings.root_url ++ "posts/" ++ (P.slug p) ++ "/"
makeCategoryUrl c = Settings.root_url ++ "categories/" ++ (C.slug c) ++ "/"

routes = [ empty                       //-> mainIndex
         , "debug/" <+/> stringParam   //-> debug
         ]

module Blog.Routes where

import qualified Blog.Category as C
import qualified Blog.Post as P
import qualified Blog.Settings as Settings

-- TODO - a better way of generating this, something like Routes

makePostUrl p = Settings.root_url ++ "posts/" ++ (P.slug p) ++ "/"
makeCategoryUrl c = Settings.root_url ++ "categories/" ++ (C.slug c) ++ "/"


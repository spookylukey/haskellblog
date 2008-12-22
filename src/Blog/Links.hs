module Blog.Links where

import qualified Blog.Category as C
import qualified Blog.Post as P
import qualified Blog.Settings as Settings

-- These need to be manually synced with Blog.Routes.  They cannot
-- live in the same module due to an import cycle when the link
-- functions are used in the templates and views.

indexUrl          = Settings.root_url
postUrl p         = Settings.root_url ++ "posts/" ++ (P.slug p) ++ "/"
categoriesUrl     = Settings.root_url ++ "categories/"
categoryUrl c     = Settings.root_url ++ "categories/" ++ (C.slug c) ++ "/"
aboutUrl          = Settings.root_url ++ "about/"
feedsUrl          = Settings.root_url ++ "feeds/"

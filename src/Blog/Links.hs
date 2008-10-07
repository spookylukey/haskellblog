module Blog.Links where

import qualified Blog.Category as C
import qualified Blog.Post as P
import qualified Blog.Settings as Settings

-- These need to be manually synced with Blog.Routes.  They cannot
-- live in the same module due to an import cycle when the link
-- functions are used in the templates and views.

indexLink          = Settings.root_url
postLink p         = Settings.root_url ++ "posts/" ++ (P.slug p) ++ "/"
categoriesLink     = Settings.root_url ++ "categories/"
categoryLink c     = Settings.root_url ++ "categories/" ++ (C.slug c) ++ "/"

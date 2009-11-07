module Blog.Links where

import qualified Blog.Category as C
import qualified Blog.Comment as Cm
import qualified Blog.Post as P
import qualified Blog.Settings as Settings
import qualified Data.ByteString.Lazy.UTF8 as UTF8

-- These need to be manually synced with Blog.Routes.  They cannot
-- live in the same module due to an import cycle when the link
-- functions are used in the templates and views.

indexUrl          = Settings.root_url
postUrl p         = Settings.root_url ++ "posts/" ++ (UTF8.toString $ P.slug p) ++ "/"
categoriesUrl     = Settings.root_url ++ "categories/"
categoryUrl c     = Settings.root_url ++ "categories/" ++ (UTF8.toString $ C.slug c) ++ "/"
loginUrl          = Settings.root_url ++ "login/"

adminMenuUrl       = Settings.root_url ++ "admin/"
adminCategoriesUrl = Settings.root_url ++ "admin/category/"
adminPostsUrl      = Settings.root_url ++ "admin/post/"
adminEditPostUrl p = Settings.root_url ++ "admin/post/edit/" ++ (show $ P.uid p) ++ "/"
adminNewPostUrl    = Settings.root_url ++ "admin/post/new/"

allPostsFeedUrl        = Settings.root_url ++ "atom/"
postCommentFeedUrl   p = Settings.root_url ++ "posts/" ++ (UTF8.toString $ P.slug p) ++ "/atom/"
categoryPostsFeedUrl c = Settings.root_url ++ "categories/" ++ (UTF8.toString $ C.slug c) ++ "/atom/"
allCommentsFeedUrl     = Settings.root_url ++ "comments/atom/"

commentUrl cm p        = postUrl p ++ "#comment" ++ (show $ Cm.uid cm)

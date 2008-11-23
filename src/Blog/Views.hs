{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Blog.Views where

import Ella.Framework (default404)
import Ella.Request
import Ella.Response
import Ella.Utils (addHtml)
import Ella.GenUtils (utf8, with)
import Blog.Templates
import Blog.Links
import Blog.DB (connect)
import Blog.Post (getPostBySlug, getCategoriesForPost)

standardResponse html = buildResponse [
                         addHtml html
                        ] utf8HtmlResponse

mainIndex :: Request -> IO (Maybe Response)
mainIndex req = return $ Just $ standardResponse mainIndexPage

debug path req = return $ Just $ buildResponse [
                  addContent "Path:\n"
                 , addContent $ utf8 path
                 , addContent "\n\nRequest:\n"
                 , addContent $ utf8 $ show req
                 ] utf8TextResponse

postsRedirectView req = return $ Just $ redirectResponse indexUrl :: IO (Maybe Response)

-- TODO

custom404 = with (standardResponse custom404page) [
             setStatus 404
            ]

dummyView req = return $ Just $ standardResponse ("TODO" :: String) :: IO (Maybe Response)

categoriesView req = return $ Just $ standardResponse categoriesPage :: IO (Maybe Response)
categoryView slug = dummyView

postView slug req = do
  cn <- connect
  mp <- getPostBySlug cn slug
  case mp of
    Nothing -> return $ Just $ custom404 -- preferred to 'Nothing'
    Just post -> do
            cats <- getCategoriesForPost cn post
            return $ Just $ standardResponse $ postPage post cats

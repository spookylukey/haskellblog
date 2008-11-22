{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Blog.Views where

import Ella.Framework (default404)
import Ella.Request
import Ella.Response
import Ella.Utils (addHtml)
import Ella.GenUtils (utf8)
import Blog.Templates
import Blog.Links
import Blog.DB (connect)
import Blog.Post (getPostBySlug)

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

postsRedirectView req = return $ Just $ redirectResponse indexLink :: IO (Maybe Response)

-- TODO

dummyView req = return $ Just $ standardResponse ("TODO" :: String) :: IO (Maybe Response)

categoriesView req = return $ Just $ standardResponse categoriesPage :: IO (Maybe Response)
categoryView slug = dummyView

postView slug req = do
  cn <- connect
  mp <- getPostBySlug cn slug
  case mp of
    Nothing -> return $ Just $ default404 -- preferred to 'Nothing'
    Just post -> return $ Just $ standardResponse $ postPage post

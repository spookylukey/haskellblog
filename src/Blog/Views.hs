{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Blog.Views where

import Ella.Framework (default404, View)
import Ella.Request
import Ella.Response
import Ella.Utils (addHtml)
import Ella.GenUtils (utf8, with, exactParse)
import Blog.Templates
import Blog.Links
import Blog.DB (connect)
import Blog.Model (getPostBySlug, getCategoriesForPost, getRecentPosts, getCommentsForPost, getRelatedPosts)
import Maybe (fromMaybe)

---- Utilities

-- | Generate a standard response, given the HTML to add.
standardResponse html = buildResponse [
                         addHtml html
                        ] utf8HtmlResponse


-- | Custom 404 response
custom404 = with (standardResponse custom404page) [
             setStatus 404
            ]

-- Temporary view
dummyView req = return $ Just $ standardResponse ("TODO" :: String) :: IO (Maybe Response)

---- Views

-- | parse a value (packed in a Just) or return a default
--
-- This is useful in dealing with 'Maybe' vals returned from
-- getGET, getPOST etc.
parseOrDefault v d = fromMaybe d (v >>= exactParse)

-- View for the main page
mainIndex :: Request -> IO (Maybe Response)
mainIndex req = do
  let page = (getGET "p" req) `parseOrDefault` 1 :: Int
  cn <- connect
  posts <- getRecentPosts cn page
  return $ Just $ standardResponse $ mainIndexPage posts

-- View to help with debugging
debug path req = return $ Just $ buildResponse [
                  addContent "Path:\n"
                 , addContent $ utf8 path
                 , addContent "\n\nRequest:\n"
                 , addContent $ utf8 $ show req
                 ] utf8TextResponse

-- View that performs redirect to main page
postsRedirectView req = return $ Just $ redirectResponse indexUrl :: IO (Maybe Response)

-- View that shows an overview of categories
categoriesView req = return $ Just $ standardResponse categoriesPage :: IO (Maybe Response)

-- View that shows posts for an individual category
categoryView :: String -> View
categoryView slug = dummyView

-- View that shows individual post
postView :: String -> View
postView slug req = do
  cn <- connect
  mp <- getPostBySlug cn slug
  case mp of
    Nothing -> return $ Just $ custom404 -- preferred to 'Nothing'
    Just post -> do
            cats <- getCategoriesForPost cn post
            comments <- getCommentsForPost cn post
            related <- getRelatedPosts cn post cats
            return $ Just $ standardResponse $ postPage post cats comments related

aboutView = dummyView

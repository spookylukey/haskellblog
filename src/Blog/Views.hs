{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Blog.Views where

import Ella.Framework (default404, View)
import Ella.Param (captureOrDefault)
import Ella.Request
import Ella.Response
import Ella.Utils (addHtml)
import Ella.GenUtils (utf8, with, exactParse)
import Blog.Templates
import Blog.Links
import Blog.DB (connect)
import Blog.Model
import Blog.Forms (CommentStage(..), validateComment)
import Maybe (fromMaybe, isJust)

---- Utilities

-- | Generate a standard response, given the HTML to add.
standardResponse html = buildResponse [
                         addHtml html
                        ] utf8HtmlResponse


-- | Custom 404 response
custom404 = with (standardResponse custom404page) [
             setStatus 404
            ]

---- Views

-- View for the main page
mainIndex :: View
mainIndex req = do
  let curpage = getPage req
  cn <- connect
  (posts,more) <- getRecentPosts cn curpage
  cats <- getCategoriesBulk cn posts
  return $ Just $ standardResponse $ mainIndexPage (zip posts cats) curpage more

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
categoriesView :: View
categoriesView req = do
  cn <- connect
  cats <- getCategories cn
  return $ Just $ standardResponse $ categoriesPage cats

-- View that shows posts for an individual category
categoryView :: String -> View
categoryView slug req = do
  let curpage = getPage req
  cn <- connect
  mcat <- getCategoryBySlug cn slug
  case mcat of
    Nothing -> return $ Just $ custom404
    Just cat -> do
              (posts,more) <- getPostsForCategory cn cat (getPage req)
              return $ Just $ standardResponse $ categoryPage cat posts curpage more

-- | View that shows individual post
postView :: String -> View
postView slug req = do
  cn <- connect
  mp <- getPostBySlug cn slug
  case mp of
    Nothing -> return $ Just $ custom404 -- preferred to 'Nothing'
    Just post -> do
            (commentStage, commentData, commentErrors) <- handleUserComment cn post req
            cats <- getCategoriesForPost cn post
            comments <- getCommentsForPost cn post
            related <- getRelatedPosts cn post cats
            return $ Just $ standardResponse $ postPage post commentStage commentData commentErrors cats comments related
  where
    handleUserComment cn post req =
        case requestMethod req of
          "POST" -> do
            (commentData, commentErrors) <- validateComment (getPOST req) post
            commentStage <-
                do
                  if isJust (getPOST req "submit")
                     then if null commentErrors
                          then
                              do
                                addComment cn commentData
                                return CommentAccepted
                          else
                              return CommentInvalid
                     -- Just assume 'preview' if not 'submit'
                     else return CommentPreview
            return (commentStage, commentData, commentErrors)

          _ -> return (NoComment, undefined, undefined)


-- | View that shows a post as a static information page -- no comments etc.
infoPageView slug req = do
  cn <- connect
  Just post <- getPostBySlug cn slug
  return $ Just $ standardResponse $ infoPage post

-- Utilities

getPage req = (getGET req "p") `captureOrDefault` 1 :: Int

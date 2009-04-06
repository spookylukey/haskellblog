{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
module Blog.Views where

-- View functions and logic. The actual HTML is found in Templates,
-- which has pure functions that generally return Html.

import Blog.DB (connect)
import Blog.Forms (CommentStage(..), validateComment, emptyComment, emptyLoginData, validateLogin)
import Blog.Links
import Blog.Model
import Blog.Templates
import Blog.Utils (getTimestamp)
import Ella.Framework (default404, View)
import Ella.GenUtils (utf8, with, exactParse)
import Ella.Param (captureOrDefault)
import Ella.Request
import Ella.Response
import Ella.Utils (addHtml)
import Maybe (fromMaybe, isJust, fromJust)
import System.Time (ClockTime(..), toUTCTime)
import qualified Blog.Settings as Settings
import qualified Data.Map as Map

---- Utilities

-- | Generate a standard response, given the HTML to add.
standardResponse html = buildResponse [
                         addHtml html
                        ] utf8HtmlResponse


-- | Custom 404 response
custom404 :: Response
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

-- | View to help with debugging
debug :: String -> View
debug path req = return $ Just $ buildResponse [
                  addContent "Path:\n"
                 , addContent $ utf8 path
                 , addContent "\n\nRequest:\n"
                 , addContent $ utf8 $ show req
                 ] utf8TextResponse

-- | View that performs redirect to main page
postsRedirectView :: View
postsRedirectView req = return $ Just $ redirectResponse indexUrl

-- | View that shows an overview of categories
categoriesView :: View
categoriesView req = do
  cn <- connect
  cats <- getCategories cn
  return $ Just $ standardResponse $ categoriesPage cats

-- | View that shows posts for an individual category
categoryView :: String -> View
categoryView slug req = do
  let curpage = getPage req
  cn <- connect
  mcat <- getCategoryBySlug cn slug
  case mcat of
    Nothing -> return $ Just $ custom404
    Just cat -> do
              (posts,more) <- getPostsForCategory cn cat (getPage req)
              cats <- getCategoriesBulk cn posts
              return $ Just $ standardResponse $ categoryPage cat (zip posts cats) curpage more

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
            if Map.null commentErrors
               then if isJust (getPOST req "submit")
                    then
                        do
                          addComment cn commentData
                          return (CommentAccepted, emptyComment, Map.empty)
                          -- Just assume 'preview' if not 'submit'
                    else return (CommentPreview, commentData, commentErrors)
               else
                   return (CommentInvalid, commentData, commentErrors)

          _ -> return (NoComment, emptyComment, Map.empty)


-- | View that shows a post as a static information page -- no comments etc.
infoPageView :: String -> View
infoPageView slug req = do
  cn <- connect
  Just post <- getPostBySlug cn slug
  return $ Just $ standardResponse $ infoPage post

-- | View that displays a login form an handles logging in
loginView :: View
loginView req = do
  cn <- connect
  loginView' cn req

-- | Testable version of loginView
loginView' cn req =
  case requestMethod req of
    "POST" -> do
      (loginData, loginErrors) <- validateLogin (getPOST req) cn
      if Map.null loginErrors
         then do
           ts <- getTimestamp
           let loginCookies = createLoginCookies loginData ts
           return $ Just $ (redirectResponse adminMenuUrl) `with` (map addCookie loginCookies)
         else
           return $ Just $ standardResponse $ loginPage loginData loginErrors
    _ -> do
      return $ Just $ standardResponse $ loginPage emptyLoginData Map.empty

standardCookie = Cookie { cookieName = ""
                        , cookieValue = ""
                        , cookieExpires = Nothing
                        , cookieDomain = Nothing
                        , cookiePath = Just "/"
                        , cookieSecure = False
                        }


createLoginCookies loginData timestamp =
  let username = fromJust $ Map.lookup "username" loginData
      password = fromJust $ Map.lookup "password" loginData
      expires = Just $ toUTCTime $ TOD (toInteger timestamp + 3600*24*365) 0
  in [ standardCookie { cookieName = "username"
                      , cookieValue = username
                      , cookieExpires = expires
                      }
     , standardCookie { cookieName = "timestamp"
                      , cookieValue = show timestamp
                      , cookieExpires = expires
                      }
     ]


adminMenuView = undefined
loginRequired = undefined

-- Utilities

getPage req = (getGET req "p") `captureOrDefault` 1 :: Int

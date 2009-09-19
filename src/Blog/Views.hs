{-# OPTIONS_GHC -fglasgow-exts -fcontext-stack=30 #-}
module Blog.Views where

-- View functions and logic. The actual HTML is found in Templates,
-- which has pure functions that generally return Html.

import Blog.DB (connect)
import Blog.Forms (CommentStage(..), validateComment, emptyComment, emptyLoginData, validateLogin, initialCommentExtra)
import Blog.Globals (mkCsrfField)
import Blog.Links
import Blog.Model
import Blog.Templates
import Ella.Framework (default404, View)
import Ella.GenUtils (utf8, with, exactParse, getTimestamp)
import Ella.Param (captureOrDefault, capture)
import Ella.Request
import Ella.Response
import Ella.Utils (addHtml)
import Maybe (fromMaybe, isJust, fromJust)
import System.Time (ClockTime(..), toUTCTime)
import Text.StringTemplate
import Text.StringTemplate.GenericStandard
import qualified Blog.Settings as Settings
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Text.XHtml as X

---- Utilities

-- | Generate a standard response, given the HTML to add.
standardResponse html = buildResponse [
                         addHtml html
                        ] utf8HtmlResponse

standardResponseBS :: LB.ByteString -> Response
standardResponseBS content = buildResponse [
                              addContent content
                             ] utf8HtmlResponse

-- | Standard response, taking a Request and StringTemplate Text as input
standardResponseTT :: Request -> StringTemplate LT.Text -> Response
standardResponseTT req template =
    let csrffield = mkCsrfField req
        t2 = setAttribute "csrffield" csrffield template
        rendered = (LT.encodeUtf8 $ render t2)
    in buildResponse [ addContent rendered
                     ] utf8HtmlResponse

return404 :: View
return404 req = do
  resp <- custom404handler req
  return $ Just $ resp

-- | Custom 404 response
custom404handler :: Request -> IO Response
custom404handler req = do
  t <- get_template "notfound"
  return $ with (standardResponseTT req t) [
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
  t <- get_template "index"
  return $ Just $ standardResponseTT req $
             (renderf t
              ("posts", map postTemplateInfo posts)
              ("categories", map (map categoryTemplateInfo) cats)
              ("paginglinks", pagingLinks indexUrl curpage more)
             )

-- | View to help with debugging
debug :: String -> View
debug path req = return $ Just $ buildResponse [
                  addContent $ utf8 "Path:\n"
                 , addContent $ utf8 path
                 , addContent $ utf8 "\n\nRequest:\n"
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
  t <- get_template "categories"
  let categories = [ (c, categoryUrl c) | c <- cats ]
  return $ Just $ standardResponseTT req $
             (renderf t
              ("categories", categories)
              ("hasCategories", not $ null cats)
             )

-- | View that shows posts for an individual category
categoryView :: String -> View
categoryView slug req = do
  let curpage = getPage req
  cn <- connect
  mcat <- getCategoryBySlug cn slug
  case mcat of
    Nothing -> return404 req
    Just cat -> do
              (posts,more) <- getPostsForCategory cn cat (getPage req)
              cats <- getCategoriesBulk cn posts
              t <- get_template "category"
              return $ Just $ standardResponseTT req $
                         (renderf t
                          ("category", cat)
                          ("posts", map postTemplateInfo posts)
                          ("categories", map (map categoryTemplateInfo) cats)
                          ("paginglinks", pagingLinks (categoryUrl cat) curpage more)
                         )

-- | View that shows individual post
postView :: String -> View
postView slug req = do
  cn <- connect
  mp <- getPostBySlug cn slug
  case mp of
    Nothing -> return404 req
    Just post -> do
            (commentStage, commentData, commentErrors, commentExtra) <- handleUserComment cn post req
            cats <- getCategoriesForPost cn post
            comments <- getCommentsForPost cn post
            related <- getRelatedPosts cn post cats
            t <- get_template "post"
            return $ Just $ standardResponseTT req $
                       (renderf t
                        ("post", postTemplateInfo post)
                        ("commentPreview", commentStage == CommentPreview)
                        ("commentAccepted", commentStage == CommentAccepted)
                        ("commentInvalid", commentStage == CommentInvalid)
                        ("newComment", commentTemplateInfo commentData)
                        ("commentErrors", Map.toList $ commentErrors)
                        ("categories", map categoryTemplateInfo cats)
                        ("comments", map commentTemplateInfo comments)
                        ("hasComments", not $ null comments)
                        ("related", map postTemplateInfo related)
                        ("hasRelated", not $ null related)
                        ("nameLabel", X.toHtml $ commentNameLabel)
                        ("nameWidget", X.toHtml $ commentNameWidget commentData)
                        ("emailLabel", X.toHtml $ commentEmailLabel)
                        ("emailWidget", X.toHtml $ commentEmailWidget commentData)
                        ("formatLabel", X.toHtml $ commentFormatLabel)
                        ("formatWidget", X.toHtml $ commentFormatWidget commentData)
                        ("messageLabel", X.toHtml $ commentMessageLabel)
                        ("messageWidget", X.toHtml $ commentMessageWidget commentData)
                        ("commentExtra", commentExtra)
                       )
  where
    handleUserComment cn post req =
        case requestMethod req of
          "POST" -> do
            creds <- getCredentials req
            (commentData, commentErrors, commentExtra) <- validateComment creds (getPOST req) post
            if Map.null commentErrors
               then if isJust (getPOST req "submit")
                    then
                        do
                          addComment cn commentData
                          return (CommentAccepted, emptyComment, Map.empty, commentExtra)
                          -- Just assume 'preview' if not 'submit'
                    else return (CommentPreview, commentData, commentErrors, commentExtra)
               else
                   return (CommentInvalid, commentData, commentErrors, commentExtra)

          _ -> do commentExtra <- initialCommentExtra req
                  return (NoComment, emptyComment, Map.empty, commentExtra)


-- | View that shows a post as a static information page -- no comments etc.
infoPageView :: String -> View
infoPageView slug req = do
  cn <- connect
  Just post <- getPostBySlug cn slug
  t <- get_template "info"
  return $ Just $ standardResponseTT req $ renderf t ("post", postTemplateInfo post)

-- | View that displays a login form and handles logging in
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
         else do
           t <- loginTemplate
           return $ Just $ standardResponseTT req $ loginPage t loginData loginErrors
    _ -> do
      t <- loginTemplate
      return $ Just $ standardResponseTT req $ loginPage t emptyLoginData (Map.empty :: Map.Map String String)

  where loginPage t loginData loginErrors =
            (renderf t
             ("loginInvalid", not $ Map.null loginErrors)
             ("loginErrors", Map.toList loginErrors)
             ("loginData", loginData)
             ("usernameLabel", X.toHtml $ loginUsernameLabel)
             ("usernameWidget", X.toHtml $ loginUsernameWidget loginData)
             ("passwordLabel", X.toHtml $ loginPasswordLabel)
             ("passwordWidget", X.toHtml $ loginPasswordWidget loginData)
            )

        loginTemplate = get_template "login"

-- | Delete auth cookies and redirect.
logoutView req =
    return $ Just $ (redirectResponse adminMenuUrl) `with` [ deleteCookie "username"
                                                           , deleteCookie "timestamp"
                                                           ]

-- Authorisation

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


timeout = 3600 * 24 * 10 -- 10 days

type Credentials = Maybe String

-- | Return the username if logged in, otherwise Nothing
--
-- Relies on secure cookies middleware
getCredentials :: Request -> IO Credentials
getCredentials req = do
  current_ts <- getTimestamp
  return $ do
    username <- getCookieVal req "username"
    timestamp <- getCookieVal req "timestamp" >>= capture
    if timestamp + timeout > current_ts
      then Just username
      else Nothing


-- Utilities

getPage req = (getGET req "p") `captureOrDefault` 1 :: Int

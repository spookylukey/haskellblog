{-# OPTIONS_GHC -fglasgow-exts -fcontext-stack=30 #-}
module Blog.Views where

-- View functions and logic. The actual HTML is found in Templates,
-- which has pure functions that generally return Html.

import Blog.DB (connect)
import Blog.Forms
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
import Maybe (fromMaybe, isJust, fromJust, catMaybes)
import System.Time (ClockTime(..), toUTCTime)
import Text.StringTemplate
import Text.StringTemplate.GenericStandard
import qualified Blog.Category as Ct
import qualified Blog.Links as Links
import qualified Blog.Post as P
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

-- | Custom 404 response
return404 :: View
return404 req = do
  t <- get_template "notfound"
  return $ Just $ with (standardResponseTT req t) [
                        setStatus 404
                       ]

return403 :: View
return403 req = do
  t <- get_template "forbidden"
  return $ Just $ with (standardResponseTT req t) [
                 setStatus 403
                ]


---- Views

-- View for the main page
mainIndex :: View
mainIndex req = do
  let curpage = getPage req
  cn <- connect
  (posts,more) <- getRecentPosts cn curpage Settings.post_page_size
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
                        ("commentErrors", commentErrors)
                        ("categories", map categoryTemplateInfo cats)
                        ("comments", map commentTemplateInfo comments)
                        ("hasComments", not $ null comments)
                        ("related", map postTemplateInfo related)
                        ("hasRelated", not $ null related)
                        ("commentData", commentData)
                        ("formatWidget", X.toHtml $ formatWidgetForComment commentData)
                        ("commentExtra", commentExtra)
                       )
  where
    handleUserComment cn post req =
        case requestMethod req of
          "POST" -> do
            creds <- getCredentials req
            (commentData, commentErrors, commentExtra) <- validateComment creds (getPOST req) post
            if null commentErrors
               then if isJust (getPOST req "submit")
                    then do
                      addComment cn commentData
                      return (CommentAccepted, emptyComment, [], commentExtra)
                    -- Just assume 'preview' if not 'submit'
                    else return (CommentPreview, commentData, commentErrors, commentExtra)
               else
                 return (CommentInvalid, commentData, commentErrors, commentExtra)

          _ -> do commentExtra <- initialCommentExtra req
                  return (NoComment, emptyComment, [], commentExtra)


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
      if null loginErrors
         then do
           ts <- getTimestamp
           let loginCookies = createLoginCookies loginData ts
           return $ Just $ (redirectResponse adminMenuUrl) `with` (map addCookie loginCookies)
         else do
           t <- loginTemplate
           return $ Just $ standardResponseTT req $ loginPage t loginData loginErrors
    _ -> do
      t <- loginTemplate
      return $ Just $ standardResponseTT req $ loginPage t emptyLoginData ([] :: [(String,String)])

  where loginPage t loginData loginErrors =
            (renderf t
             ("loginInvalid", not $ null loginErrors)
             ("loginErrors", loginErrors)
             ("loginData", loginData)
            )

        loginTemplate = get_template "login"

-- | Delete auth cookies and redirect.
logoutView req =
    return $ Just $ (redirectResponse adminMenuUrl) `with` [ deleteCookie "username"
                                                           , deleteCookie "timestamp"
                                                           ]

--
-- Admin views
--

-- Category editing is very simple and doesn't require
-- much validation.

adminMenu :: View
adminMenu req = do
  t <- get_template "admin_menu"
  return $ Just $ standardResponseTT req $
         (renderf t
          ("pagetitle", "Blog admin - menu")
          ("adminNewPostUrl", Links.adminNewPostUrl)
          ("adminPostsUrl", Links.adminPostsUrl)
          ("adminCategoriesUrl", Links.adminCategoriesUrl)
         )

adminPosts req = do
  t <- get_template "admin_posts"
  let curpage = getPage req
  cn <- connect
  (posts,more) <- getRecentPosts cn curpage Settings.admin_post_page_size
  return $ Just $ standardResponseTT req $
             (renderf t
              ("pagetitle", "Edit posts")
              ("posts", map postTemplateInfo posts)
              ("paginglinks", pagingLinks Links.adminPostsUrl curpage more)
             )

-- | View that handles all editing of categories (add/edit/delete)
adminCategories req = do
  cn <- connect
  t <- get_template "admin_categories"
  -- handle deletion if "delete" in POST vars
  -- handle adding/editing if "save" in POST vars
  message <- handlePost req cn
  categories <- getCategories cn
  return $ Just $ standardResponseTT req $
         (renderf t
          ("categories", categories)
          ("message", message)
          ("showMessage", length message > 0)
         )
  where
      handlePost req cn =
          if requestMethod req == "POST"
          then if isJust (getPOST req "save")
               then
                   let catid = (getPOST req "catid") `captureOrDefault` 0 :: Int
                   in if catid == 0
                      then do
                        let ct = Ct.newCategory (getPOST req "name" `captureOrDefault` "")
                        addCategory cn ct
                        return "Category added"
                      else do
                        Just ct <- getCategoryById cn catid
                        let ct2 = ct { Ct.name = (getPOST req "name" `captureOrDefault` "") }
                        updateCategory cn ct2
                        return ("Category " ++ show catid ++ " saved")
               else if isJust (getPOST req "delete")
                    then
                        let catid = (getPOST req "categories") `captureOrDefault` 0 :: Int
                        in do
                          deleteCategory cn catid
                          return ("Category " ++ show catid ++ " deleted")
                    else return ""
          else return ""

-- | View that handles editing an existing blog post
adminEditPost post_id req = do
  cn <- connect
  m_post <- getPostById cn post_id
  case m_post of
    Just p -> adminEditPost' p False cn req
    Nothing -> return404 req

-- | View that handles adding a new blog post
adminNewPost req = do
  cn <- connect
  adminEditPost' emptyPost True cn req

adminEditPost' post isNew cn req = do
  categories <- getCategories cn
  postCategories <- if isNew then return [] else getCategoriesForPost cn post
  case requestMethod req of
    "GET" ->  output post (map Ct.uid postCategories) categories "start" []
    "POST" -> do
      let mode = head $ map fst $ filter snd $  [ ("submit", hasPOST req "submit")
                                                , ("delete", hasPOST req "delete")
                                                -- use preview as default, for simplicity
                                                , ("preview", True)
                                                ]
      if mode == "delete"
        then do
          deletePost cn (P.uid post)
          return $ Just $ redirectResponse adminMenuUrl
        else do
          (postData, postCatIds, postErrors) <- validatePost req post
          if null postErrors
            then
              if mode == "submit"
                then do
                  if isNew
                    then do
                      -- Set timestamp here, because we don't want to do it in
                      -- validatePost (we would need to pass in isNew)
                      ts <- getTimestamp
                      let newPost = postData { P.timestamp = ts }
                      addPost cn newPost postCatIds
                    else updatePost cn postData postCatIds
                  return $ Just $ redirectResponse adminMenuUrl
              else do
                -- mode == "preview"
                output postData postCatIds categories mode postErrors
            else
                -- invalid
                output postData postCatIds categories "invalid" postErrors
  where
    output :: P.Post -> [Int] -> [Ct.Category] -> String -> [(String, String)] -> IO (Maybe Response)
    output postData postCatIds categories mode errors =
        do
          t <- get_template "admin_post"
          return $ Just $ standardResponseTT req $
                                   (renderf t
                                    ("post", postData)
                                    ("categoriesWidget", X.toHtml $ categoriesWidgetForPost postCatIds categories)
                                    ("formatWidget", X.toHtml $ formatWidgetForPost postData)
                                    ("isNew", isNew)
                                    ("pagetitle", if isNew then "Add post" else "Edit post")
                                    ("mode", mode)
                                    ("errors", errors)
                                    ("showErrors", not $ null errors)
                                    ("showPreview", mode == "preview")
                                   )

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


-- Decorators

-- | Decorate a view function with this to limit the view
-- to users who are 'admins'

adminRequired :: View -> View
adminRequired view = \req -> do
  creds <- getCredentials req
  case creds of
    Just n -> if n `elem` Settings.admin_usernames
              then view req
              else return403 req
    Nothing -> return403 req

-- Utilities

getPage req = (getGET req "p") `captureOrDefault` 1 :: Int

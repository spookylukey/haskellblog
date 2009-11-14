module Blog.Model ( addPost
                  , updatePost
                  , deletePost
                  , addCategory
                  , updateCategory
                  , deleteCategory
                  , addPostCategory
                  , addComment
                  , deleteComment
                  , createUser
                  , getPostBySlug
                  , getPostById
                  , getRecentPosts
                  , getRecentComments
                  , getCategoriesForPost
                  , getCommentsForPost
                  , getRelatedPosts
                  , getCategoryBySlug
                  , getCategoryById
                  , getCategories
                  , getCategoriesBulk
                  , getPostsForCategory
                  , setPassword
                  , checkPassword
                  , setCommentVisible
                  , setCommentResponse
                  , getSpamWords
                  , addSpamWord
                  , deleteSpamWord
                  ) where

import Data.Digest.Pure.SHA (showDigest, sha1)
import Database.HDBC
import Blog.DBUtils (makeSlugGeneric, pagedQuery, sqlInIds, getDbId)
import Blog.Utils (regexReplace, split)
import Ella.GenUtils (utf8, randomStr)
import qualified Blog.DB as DB
import qualified Blog.Post as P
import qualified Blog.Category as Ct
import qualified Blog.Comment as Cm
import qualified Blog.Settings as Settings
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8 as UTF8

------ Create/Update/Delete -------

-- post table
postColumnNames = [ "title"
                  , "slug"
                  , "post_raw"
                  , "post_formatted"
                  , "summary_raw"
                  , "summary_formatted"
                  , "format_id"
                  , "timestamp"
                  , "comments_open"
                  ]
postColumnValues p = [ toSql $ P.title p
                     , toSql $ P.slug p
                     , toSql $ P.post_raw p
                     , toSql $ P.post_formatted p
                     , toSql $ P.summary_raw p
                     , toSql $ P.summary_formatted p
                     , toSql $ fromEnum $ P.format p
                     , toSql $ P.timestamp p
                     , toSql $ P.comments_open p
                     ]

addPost cn p catIds = do
  theslug <- makePostSlug cn p
  let p2 = p { P.slug = utf8 theslug }
  DB.doInsert cn "posts" postColumnNames (postColumnValues p2)
  newid <- getDbId cn
  setPostCategories cn newid catIds
  return p2 { P.uid = newid }

makePostSlug cn p = makeSlugGeneric cn (UTF8.toString $ P.title p) "posts"

updatePost cn p catIds = do
  DB.doUpdate cn "posts" postColumnNames (postColumnValues p)
        "WHERE id = ?" [toSql $ P.uid p]
  setPostCategories cn (P.uid p) catIds
  return p

deletePost cn uid = do
  deletePostCategoriesForPost cn uid
  DB.doDelete cn "posts" "WHERE id = ?" [toSql uid]

-- category table
categoryColumnNames = [ "name"
                      , "slug"
                      ]
categoryColumnValues c = [ toSql $ Ct.name c
                         , toSql $ Ct.slug c
                         ]


addCategory cn c =  do theslug <- makeCategorySlug cn c
                       let c2 = c { Ct.slug = utf8 theslug }
                       DB.doInsert cn "categories" categoryColumnNames (categoryColumnValues c2)
                       newid <- getDbId cn
                       return c2 { Ct.uid = newid }

makeCategorySlug cn cat = makeSlugGeneric cn (UTF8.toString $ Ct.name cat) "categories"

updateCategory cn c = do
  DB.doUpdate cn "categories" categoryColumnNames (categoryColumnValues c)
        "WHERE id = ?" [ toSql $ Ct.uid c]

deleteCategory cn uid = do
  deletePostCategoriesForCat cn uid
  DB.doDelete cn "categories" "WHERE id = ?" [toSql uid]

-- post_categories tables
addPostCategory :: (IConnection conn) => conn -> (Int, Int) -> IO (Int, Int)
addPostCategory cn pc = do { DB.doInsert cn "post_categories"
                             ["post_id",
                              "category_id"]
                             [toSql $ fst pc,
                              toSql $ snd pc];
                             return pc; }

deletePostCategoriesForCat cn catId =
  DB.doDelete cn "post_categories" "WHERE category_id = ?" [toSql catId]

deletePostCategoriesForPost cn postId =
  DB.doDelete cn "post_categories" "WHERE post_id = ?" [toSql postId]

setPostCategories cn postId catIds = do
  deletePostCategoriesForPost cn postId
  mapM_ (\c -> addPostCategory cn (postId, c)) catIds


-- comment table
commentColumnNames = [ "post_id"
                     , "timestamp"
                     , "name"
                     , "email"
                     , "text_raw"
                     , "text_formatted"
                     , "format_id"
                     , "hidden"
                     , "response"
                     ]
commentColumnValues cm = [ toSql $ Cm.post_id cm
                         , toSql $ Cm.timestamp cm
                         , toSql $ Cm.name cm
                         , toSql $ Cm.email cm
                         , toSql $ Cm.text_raw cm
                         , toSql $ Cm.text_formatted cm
                         , toSql $ fromEnum $ Cm.format cm
                         , toSql $ Cm.hidden cm
                         , toSql $ Cm.response cm
                         ]

addComment cn cm = do
  DB.doInsert cn "comments" commentColumnNames (commentColumnValues cm)
  newid <- getDbId cn
  return cm { Cm.uid = newid }

deleteComment cn uid = do
  DB.doDelete cn "comments" "WHERE id = ?" [toSql uid]

-- user table
userColumnNames = [ "username"
                  , "password"
                  , "superuser"
                  ]

createUser :: (IConnection conn) =>
              conn -> String -> Bool -> IO Int
createUser cn username superuser = do
  DB.doInsert cn "users" userColumnNames
        [ toSql username
        , toSql ""
        , toSql superuser
        ]
  newid <- getDbId cn
  return newid

-------- Queries -----------

---- Statements -----

-- We optimise queries by removing items that are not actually used and replacing them with ''
-- (we can then use the same 'makePost' function)
getPostByIdQuery        = "SELECT id, title, slug, post_raw, post_formatted, summary_raw, summary_formatted, format_id, timestamp, comments_open FROM posts WHERE id = ?;"
getPostBySlugQuery      = "SELECT id, title, slug, '',       post_formatted, '',          '',                '',        timestamp, comments_open FROM posts WHERE slug = ?;"
getRecentPostsQuery     = "SELECT id, title, slug, '',       post_formatted, '',          summary_formatted, '',        timestamp, ''            FROM posts ORDER BY timestamp DESC $LIMITOFFSET;"
getPostsForCategoryQuery= "SELECT id, title, slug, '',       post_formatted, '',          summary_formatted, '',        timestamp, ''            FROM posts INNER JOIN post_categories ON posts.id = post_categories.post_id WHERE post_categories.category_id = ? ORDER BY timestamp DESC $LIMITOFFSET;"

-- Used to get post related to a post, ordered to favour posts with
-- more matching categories and close in time to the original post
getRelatedPostsQuery ids = "SELECT id, title, slug, '',       '',             '',          '',                '',               '', ''            FROM posts INNER JOIN (SELECT post_id, COUNT(post_id) AS c from post_categories WHERE category_id IN " ++ sqlInIds ids ++ " GROUP BY post_id) as t2 ON posts.id = t2.post_id AND posts.id <> ? ORDER BY c DESC, abs(posts.timestamp - ?) ASC $LIMITOFFSET;"

getCategoryBySlugQuery    = "SELECT categories.id, categories.name, categories.slug FROM categories WHERE slug = ?;"
getCategoryByIdQuery      = "SELECT categories.id, categories.name, categories.slug FROM categories WHERE id = ?;"
getCategoriesQuery        = "SELECT categories.id, categories.name, categories.slug FROM categories ORDER BY slug;"
getCategoriesForPostQuery = "SELECT categories.id, categories.name, categories.slug FROM categories INNER JOIN post_categories ON categories.id = post_categories.category_id WHERE post_categories.post_id = ? ORDER BY categories.slug;"
getCategoriesBulkQuery ids= "SELECT categories.id, categories.name, categories.slug, post_categories.post_id FROM categories INNER JOIN post_categories ON categories.id = post_categories.category_id WHERE post_categories.post_id IN " ++ sqlInIds ids ++ " ORDER BY categories.slug;"

getCommentByIdQuery      = "SELECT id, post_id, timestamp, name, email, text_raw, text_formatted, format_id, hidden, response FROM comments WHERE id = ?;"
getCommentsForPostQuery  = "SELECT id, '',      timestamp, name, email, '',       text_formatted, '',        hidden, response FROM comments WHERE post_id = ? ORDER BY timestamp ASC;"

getRecentCommentsQuery   = "SELECT c.id, c.post_id, c.timestamp, c.name, c.email, '',       c.text_formatted, '',        c.hidden, c.response, p.slug as post_slug, p.title as post_title FROM comments as c INNER JOIN posts as p ON c.post_id = p.id ORDER BY c.timestamp DESC $LIMITOFFSET ;"

getPasswordForUsernameQuery = "SELECT password FROM users WHERE username = ?;"
setPasswordForUsernameQuery = "UPDATE users SET password = ? WHERE username = ?;"

setCommentHiddenQuery      = "UPDATE comments SET hidden = ? WHERE id = ?;"
setCommentResponseQuery    = "UPDATE comments SET response = ? WHERE id = ?;"

getSpamWordsQuery          = "SELECT word FROM spamwords;"
addSpamWordQuery           = "INSERT into spamwords (word) VALUES (?);"
deleteSpamWordQuery        = "DELETE from spamwords WHERE word = ?;"

---- Constructors ----

makePost row =
    P.Post { P.uid = fromSql (row !! 0)
           , P.title = fromSql (row !! 1)
           , P.slug = fromSql (row !! 2)
           , P.post_raw = fromSql (row !! 3)
           , P.post_formatted = fromSql (row !! 4)
           , P.summary_raw = fromSql (row !! 5)
           , P.summary_formatted = fromSql (row !! 6)
           , P.format = toEnum $ fromSql (row !! 7)
           , P.timestamp = fromSql (row !! 8)
           , P.comments_open = fromSql (row !! 9)
           }

makeCategory row =
    Ct.Category { Ct.uid = fromSql (row !! 0)
                , Ct.name = fromSql (row !! 1)
                , Ct.slug = fromSql (row !! 2)
                }

makeComment row =
    Cm.Comment { Cm.uid = fromSql (row !! 0)
               , Cm.post_id = fromSql (row !! 1)
               , Cm.timestamp = fromSql (row !! 2)
               , Cm.name = fromSql (row !! 3)
               , Cm.email = fromSql (row !! 4)
               , Cm.text_raw = fromSql (row !! 5)
               , Cm.text_formatted = fromSql (row !! 6)
               , Cm.format = toEnum $ fromSql (row !! 7)
               , Cm.hidden = fromSql (row !! 8)
               , Cm.response = fromSql (row !! 9)
               }

minimalPost slug title =
    P.Post { P.uid = undefined
           , P.title = title
           , P.slug = slug
           , P.post_raw = undefined
           , P.post_formatted = undefined
           , P.summary_raw = undefined
           , P.summary_formatted = undefined
           , P.format = undefined
           , P.timestamp = undefined
           , P.comments_open = undefined
           }


---- Public API for queries ----

getPostBySlug :: (IConnection conn) => conn -> String -> IO (Maybe P.Post)
getPostBySlug cn slug = do
  res <- quickQuery' cn getPostBySlugQuery [toSql slug]
  case res of
    [] -> return Nothing
    (postdata:_) -> return $ Just $ makePost postdata

getPostById :: (IConnection conn) => conn -> Int -> IO (Maybe P.Post)
getPostById cn postid = do
  res <- quickQuery' cn getPostByIdQuery [toSql postid]
  case res of
    [] -> return Nothing
    (postdata:_) -> return $ Just $ makePost postdata

getRecentPosts :: (IConnection conn) => conn -> Int -> Int -> IO ([P.Post], Bool)
getRecentPosts cn page pagesize = do
  (res,more) <- pagedQuery cn getRecentPostsQuery [] page pagesize
  return (map makePost res, more)

getPostsForCategory :: (IConnection conn) => conn -> Ct.Category -> Int -> Int -> IO ([P.Post], Bool)
getPostsForCategory cn cat page pagesize = do
  (res,more) <- pagedQuery cn getPostsForCategoryQuery [toSql $ Ct.uid cat] page pagesize
  return (map makePost res, more)

-- | Returns all recent comments, paired with the Post they are from
-- Contains only enough information to generate the feed and the 'Recent comments' page
getRecentComments :: (IConnection conn) => conn -> Int -> Int -> IO ([(Cm.Comment, P.Post)],Bool)
getRecentComments cn page pagesize = do
  (res, more) <- pagedQuery cn getRecentCommentsQuery [] page pagesize
  let comments = map makeComment res
  let posts = map (\row -> minimalPost (fromSql $ row !! 10) (fromSql $ row !! 11)) res
  return $ (zip comments posts, more)

getCategoryBySlug :: (IConnection conn) => conn -> String -> IO (Maybe Ct.Category)
getCategoryBySlug cn slug = do
  res <- quickQuery' cn getCategoryBySlugQuery [toSql slug]
  case res of
    [] -> return Nothing
    (rs:_) -> return $ Just $ makeCategory rs

getCategoryById :: (IConnection conn) => conn -> Int -> IO (Maybe Ct.Category)
getCategoryById cn catid = do
  res <- quickQuery' cn getCategoryByIdQuery [toSql catid]
  case res of
    [] -> return Nothing
    (rs:_) -> return $ Just $ makeCategory rs

getCategories :: (IConnection conn) => conn -> IO [Ct.Category]
getCategories cn = do
  res <- quickQuery' cn getCategoriesQuery []
  return $ map makeCategory res

getCategoriesForPost :: (IConnection conn) => conn -> P.Post -> IO [Ct.Category]
getCategoriesForPost cn post = do
  res <- quickQuery' cn getCategoriesForPostQuery [toSql $ P.uid post]
  return $ map makeCategory res

getCommentsForPost :: (IConnection conn) => conn -> P.Post -> IO [Cm.Comment]
getCommentsForPost cn post = do
  res <- quickQuery' cn getCommentsForPostQuery [toSql $ P.uid post]
  return $ map makeComment res

-- | Gets the categories for a list of posts.  Results are returned
-- as a list of list of categories
getCategoriesBulk :: (IConnection conn) =>
                     conn                  -- ^ connection
                  -> [P.Post]              -- ^ list of posts
                  -> IO [[Ct.Category]]
getCategoriesBulk cn posts = do
  let ids = map (P.uid) posts
  res <- quickQuery' cn (getCategoriesBulkQuery ids) []
  -- Create (Category, post id) pairs:
  let cats = map (\r -> (makeCategory r, (fromSql $ r !! 3) :: Int)) res
  -- split them up according to ids
  return [ [ cat | (cat, pid) <- cats, P.uid p == pid ] | p <- posts]


getRelatedPosts :: (IConnection conn) =>
                   conn -> P.Post -> [Ct.Category] -> IO [P.Post]
getRelatedPosts cn post categories = do
  let ids = map (Ct.uid) categories
  (res,_) <- pagedQuery cn (getRelatedPostsQuery ids) [ toSql $ P.uid post
                                                      , toSql $ P.timestamp post ] 1 7
  return $ map makePost res

makePasswordHash password = do
  salt <- randomStr 10
  let digest = sha1 $ utf8 (salt ++ password)
  return ("sha1:" ++ salt ++ ":" ++ (showDigest $ digest))

checkPasswordHash salt hash password =
    (showDigest $ sha1 $ utf8 (salt ++ password)) == hash

-- | Checks that the password for a user is correct
checkPassword :: (IConnection conn) =>
                 conn -> String -> String -> IO Bool
checkPassword cn username password = do
  res <- quickQuery' cn getPasswordForUsernameQuery [ toSql username ]
  if null res
     then return False
     else do
         [[SqlByteString pwdData]] <- return res -- force pattern match
         -- pwdData stores algo;salt;hash
         ["sha1", salt, hash] <- return $ split (UTF8.toString $ BL.fromChunks [pwdData]) ':'
         return $ checkPasswordHash salt hash password

-- | Sets the password for a user
setPassword :: (IConnection conn) =>
               conn -> String -> String -> IO ()
setPassword cn username password = do
  pwdHash <- makePasswordHash password
  withTransaction cn (\cn ->
                      run cn setPasswordForUsernameQuery [ toSql pwdHash
                                                         , toSql username ]
                     )
  return ()


-- comment moderation
setCommentVisible :: (IConnection conn) =>
               conn -> Int -> Bool -> IO ()
setCommentVisible cn commentId visible = do
    withTransaction cn (\cn ->
                            run cn setCommentHiddenQuery [ toSql (if visible then 0 else 1 :: Int)
                                                         , toSql commentId ]
                       )
    return ()

setCommentResponse :: (IConnection conn) =>
                      conn -> Int -> String -> IO ()
setCommentResponse cn commentId response = do
  withTransaction cn (\cn ->
                          run cn setCommentResponseQuery [ toSql response
                                                         , toSql commentId
                                                         ]
                     )
  return ()

getSpamWords :: (IConnection conn) => conn -> IO [String]
getSpamWords cn = do
  res <- quickQuery' cn getSpamWordsQuery []
  return [fromSql $ row !! 0 | row <- res]

addSpamWord :: (IConnection conn) => conn -> String -> IO ()
addSpamWord cn word = do
  withTransaction cn (\cn ->
                          run cn addSpamWordQuery [ toSql word ]
                     )
  return ()

deleteSpamWord :: (IConnection conn) => conn -> String -> IO ()
deleteSpamWord cn word = do
  withTransaction cn (\cn ->
                          run cn deleteSpamWordQuery [ toSql word ]
                     )
  return ()

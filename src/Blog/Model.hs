module Blog.Model ( addPost
                  , addCategory
                  , addPostCategory
                  , addComment
                  , getPostBySlug
                  , getRecentPosts
                  , getCategoriesForPost
                  , getCommentsForPost
                  , getRelatedPosts
                  , getCategories
                  , getCategoriesBulk
                  ) where

import Database.HDBC
import Blog.DBUtils (makeSlugGeneric, pagedQuery, sqlInIds, getDbId)
import Blog.Utils (regexReplace)
import qualified Blog.DB as DB
import qualified Blog.Post as P
import qualified Blog.Category as Ct
import qualified Blog.Comment as Cm
import qualified Data.ByteString.Lazy.Char8 as BL

------ Create -------
addPost cn p = do theslug <- makePostSlug cn p
                  let p2 = p { P.slug = theslug }
                  DB.doInsert cn "posts" [
                         "title",
                         "slug",
                         "post_raw",
                         "post_formatted",
                         "summary_raw",
                         "summary_formatted",
                         "format_id",
                         "timestamp",
                         "comments_open"
                        ] [
                         toSql $ P.title p2,
                         toSql $ P.slug p2,
                         toSql $ P.post_raw p2,
                         toSql $ P.post_formatted p2,
                         toSql $ P.summary_raw p2,
                         toSql $ P.summary_formatted p2,
                         toSql $ P.format_id p2,
                         toSql $ P.timestamp p2,
                         toSql $ P.comments_open p2
                        ]
                  newid <- getDbId cn
                  return p2 { P.uid = newid }

makePostSlug cn p = makeSlugGeneric cn (P.title p) "posts"

addCategory cn c =  do theslug <- makeCategorySlug cn c
                       let c2 = c { Ct.slug = theslug }
                       DB.doInsert cn "categories"
                             ["name",
                              "slug"]
                             [toSql $ Ct.name c2,
                              toSql $ Ct.slug c2]
                       newid <- getDbId cn
                       return c2 { Ct.uid = newid }

makeCategorySlug cn cat = makeSlugGeneric cn (Ct.name cat) "categories"

addPostCategory cn pc = do { DB.doInsert cn "post_categories"
                             ["post_id",
                              "category_id"]
                             [toSql $ fst pc,
                              toSql $ snd pc];
                             return pc; }


addComment cn cm = do
  DB.doInsert cn "comments" [
                    "post_id"
                   , "timestamp"
                   , "name"
                   , "email"
                   , "text_raw"
                   , "text_formatted"
                   , "format_id"
                   ] [
                    toSql $ Cm.post_id cm
                   , toSql $ Cm.timestamp cm
                   , toSql $ Cm.name cm
                   , toSql $ Cm.email cm
                   , toSql $ Cm.text_raw cm
                   , toSql $ Cm.text_formatted cm
                   , toSql $ Cm.format_id cm
                   ]
  newid <- getDbId cn
  return cm { Cm.uid = newid }

-------- Queries -----------

---- Statements -----

-- We optimise queries by removing items that are not actually used and replacing them with ''
-- (we can then use the same 'makePost' function)
getPostByIdQuery        = "SELECT id, title, slug, post_raw, post_formatted, summary_raw, summary_formatted, format_id, timestamp, comments_open FROM posts WHERE id = ?;"
getPostBySlugQuery      = "SELECT id, title, slug, '',       post_formatted, '',          '',                '',        timestamp, comments_open FROM posts WHERE slug = ?;"
getRecentPostsQuery     = "SELECT id, title, slug, '',       '',             '',          summary_formatted, '',        timestamp, ''            FROM posts ORDER BY timestamp DESC $LIMITOFFSET;"


-- Used to get post related to a post, ordered to favour posts with
-- more matching categories and close in time to the original post
getRelatedPostsQuery ids = "SELECT id, title, slug, '',       '',             '',          '',                '',               '', ''            FROM posts INNER JOIN (SELECT post_id, COUNT(post_id) AS c from post_categories WHERE category_id IN " ++ sqlInIds ids ++ " GROUP BY post_id) as t2 ON posts.id = t2.post_id AND posts.id <> ? ORDER BY c DESC, abs(posts.timestamp - ?) ASC $LIMITOFFSET;"

getCategoriesQuery        = "SELECT categories.id, categories.name, categories.slug FROM categories ORDER BY slug;"
getCategoriesForPostQuery = "SELECT categories.id, categories.name, categories.slug FROM categories INNER JOIN post_categories ON categories.id = post_categories.category_id WHERE post_categories.post_id = ? ORDER BY categories.slug;"
getCategoriesBulkQuery ids= "SELECT categories.id, categories.name, categories.slug, post_categories.post_id FROM categories INNER JOIN post_categories ON categories.id = post_categories.category_id WHERE post_categories.post_id IN " ++ sqlInIds ids ++ " ORDER BY categories.slug;"

getCommentByIdQuery      = "SELECT id, post_id, timestamp, name, email, text_raw, text_formatted, format_id FROM comments WHERE id = ?;"
getCommentsForPostQuery  = "SELECT id, '',      timestamp, name, email, '',       text_formatted, ''        FROM comments WHERE post_id = ? ORDER BY timestamp ASC;"


---- Constructors ----

makePost row =
    P.Post { P.uid = fromSql (row !! 0)
           , P.title = fromSql (row !! 1)
           , P.slug = fromSql (row !! 2)
           , P.post_raw = fromSql (row !! 3)
           , P.post_formatted = fromSql (row !! 4)
           , P.summary_raw = fromSql (row !! 5)
           , P.summary_formatted = fromSql (row !! 6)
           , P.format_id = fromSql (row !! 7)
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
               , Cm.format_id = fromSql (row !! 7)
               }

---- Public API for queries ----

getPostBySlug :: (SqlType a, IConnection conn) => conn -> a -> IO (Maybe P.Post)
getPostBySlug cn slug = do
  res <- quickQuery' cn getPostBySlugQuery [toSql slug]
  case res of
    [] -> return Nothing
    (postdata:_) -> return $ Just $ makePost postdata

getRecentPosts :: (IConnection conn) => conn -> Int -> IO ([P.Post], Bool)
getRecentPosts cn page = do
  (res,more) <- pagedQuery cn getRecentPostsQuery [] page 20
  return (map makePost res, more)

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

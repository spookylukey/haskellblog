module Blog.Model ( addPost
                  , addCategory
                  , addPostCategory
                  , addComment
                  , getPostBySlug
                  , getRecentPosts
                  , getCategoriesForPost
                  , getCommentsForPost
                  ) where

import Database.HDBC
import Blog.DBUtils (makeSlugGeneric)
import qualified Blog.DB as DB
import qualified Blog.Post as P
import qualified Blog.Category as Ct
import qualified Blog.Comment as Cm

------ Create -------
getDbId cn =
    do
      [[newid]] <- quickQuery' cn "SELECT last_insert_rowid();" []
      return $ fromSql newid

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
getRecentPostQueries    = "SELECT id, title, slug, '',       '',             '',          summary_formatted, '',        timestamp, ''            FROM posts ORDER BY timestamp DESC LIMIT 20;"

getCategoriesForPostQuery = "SELECT categories.id, categories.name, categories.slug FROM categories INNER JOIN post_categories ON categories.id = post_categories.category_id WHERE post_categories.post_id = ? ORDER BY categories.slug;"

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

getPostBySlug cn slug = do
  res <- quickQuery' cn getPostBySlugQuery [toSql slug]
  case res of
    [] -> return Nothing
    (postdata:_) -> return $ Just $ makePost postdata

getRecentPosts cn = do
  res <- quickQuery' cn getRecentPostQueries []
  return $ map makePost res

getCategoriesForPost cn post = do
  res <- quickQuery' cn getCategoriesForPostQuery [toSql $ P.uid post]
  return $ map makeCategory res

getCommentsForPost cn post = do
  res <- quickQuery' cn getCommentsForPostQuery [toSql $ P.uid post]
  return $ map makeComment res

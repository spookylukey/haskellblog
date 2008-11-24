module Blog.Model ( addPost
                  , addCategory
                  , makePostSlug
                  , addPostCategory
                  , getPostBySlug
                  , getRecentPosts
                  , getCategoriesForPost
                  ) where

import Database.HDBC
import Blog.DBUtils (makeSlugGeneric)
import qualified Blog.DB as DB
import qualified Blog.Post as P
import qualified Blog.Category as Ct
import qualified Blog.Comment as Cm

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
                  [[newid]] <- quickQuery' cn "SELECT last_insert_rowid();" []
                  return p2 { P.uid = fromSql $ newid }

makePostSlug cn p = makeSlugGeneric cn (P.title p) "posts"

addCategory cn c =  do theslug <- makeCategorySlug cn c
                       let c2 = c { Ct.slug = theslug }
                       DB.doInsert cn "categories"
                             ["name",
                              "slug"]
                             [toSql $ Ct.name c2,
                              toSql $ Ct.slug c2]
                       [[newid]] <- quickQuery cn "SELECT last_insert_rowid();" [];
                       return c2 { Ct.uid = fromSql $ newid }

makeCategorySlug cn cat = makeSlugGeneric cn (Ct.name cat) "categories"

addPostCategory cn pc = do { DB.doInsert cn "post_categories"
                             ["post_id",
                              "category_id"]
                             [toSql $ fst pc,
                              toSql $ snd pc];
                             return pc; }

-------- Queries -----------

---- Statements -----

-- We optimise queries by removing items that are not actually used and replacing them with ''
-- (we can then use the same 'makePost' function)
getPostByIdQuery        = "SELECT id, title, slug, post_raw, post_formatted, summary_raw, summary_formatted, format_id, timestamp, comments_open FROM posts WHERE id = ?;"
getPostBySlugQuery      = "SELECT id, title, slug, '',       post_formatted, '',          '',                '',        timestamp, comments_open FROM posts WHERE slug = ?;"
getRecentPostQueries    = "SELECT id, title, slug, '',       '',             '',          summary_formatted, '',        timestamp, ''            FROM posts ORDER BY timestamp DESC LIMIT 20;"

getCategoriesForPostQuery = "SELECT categories.id, categories.name, categories.slug FROM categories INNER JOIN post_categories ON categories.id = post_categories.category_id WHERE post_categories.post_id = ? ORDER BY categories.slug;"


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

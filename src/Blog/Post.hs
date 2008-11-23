module Blog.Post where

import Database.HDBC
import Blog.DBUtils (makeSlugGeneric)
import qualified Blog.DB as DB
import qualified Blog.Category as C

data Post = Post {
      uid :: Int,
      title :: String,
      slug :: String,
      post_raw :: String,
      post_formatted :: String,
      summary_raw :: String,
      summary_formatted :: String,
      format_id :: Int,
      timestamp :: Int,
      comments_open :: Bool
    } deriving (Show, Eq)

addPost cn p = do theslug <- makePostSlug cn p
                  let p2 = p { slug = theslug }
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
                         toSql $ title p2,
                         toSql $ slug p2,
                         toSql $ post_raw p2,
                         toSql $ post_formatted p2,
                         toSql $ summary_raw p2,
                         toSql $ summary_formatted p2,
                         toSql $ format_id p2,
                         toSql $ timestamp p2,
                         toSql $ comments_open p2
                        ]
                  [[newid]] <- quickQuery cn "SELECT last_insert_rowid();" []
                  return p2 { uid = fromSql $ newid }

makePostSlug cn p = makeSlugGeneric cn (title p) "posts"

addPostCategory cn pc = do { DB.doInsert cn "post_categories"
                             ["post_id",
                              "category_id"]
                             [toSql $ fst pc,
                              toSql $ snd pc];
                             return pc; }


-- We optimise queries by removing items that are not actually used and replacing them with ''
-- (we can then use the same 'makePost' function)
getPostByIdQuery        = "SELECT id, title, slug, post_raw, post_formatted, summary_raw, summary_formatted, format_id, timestamp, comments_open FROM posts WHERE id = ?;"
getPostBySlugQuery      = "SELECT id, title, slug, '',       post_formatted, '',          '',                '',        timestamp, comments_open FROM posts WHERE slug = ?;"
getRecentPostQueries    = "SELECT id, title, slug, '',       '',             '',          summary_formatted, '',        timestamp, ''            FROM posts ORDER BY timestamp DESC LIMIT 20;"

getCategoriesForPostQuery = "SELECT categories.id, categories.name, categories.slug FROM categories INNER JOIN post_categories ON categories.id = post_categories.category_id WHERE post_categories.post_id = ?;"

getPostBySlug cn slug = do
  res <- quickQuery cn getPostBySlugQuery [toSql slug]
  case res of
    [] -> return Nothing
    (postdata:_) -> return $ Just $ makePost postdata

makePost row =
    Post { uid = fromSql (row !! 0)
         , title = fromSql (row !! 1)
         , slug = fromSql (row !! 2)
         , post_raw = fromSql (row !! 3)
         , post_formatted = fromSql (row !! 4)
         , summary_raw = fromSql (row !! 5)
         , summary_formatted = fromSql (row !! 6)
         , format_id = fromSql (row !! 7)
         , timestamp = fromSql (row !! 8)
         , comments_open = fromSql (row !! 9)
         }

getRecentPosts cn = do
  res <- quickQuery cn getRecentPostQueries []
  return $ map makePost res

getCategoriesForPost cn post = do
  res <- quickQuery cn getCategoriesForPostQuery [toSql $ uid post]
  return $ map C.makeCategory res

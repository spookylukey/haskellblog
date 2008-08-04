import qualified Category as C
import qualified Post as P
import qualified Settings
import qualified DB
import qualified Formats
import Database.HDBC
import List (sortBy)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Monad (liftM)
-- Migration script for the old data

-- Read a table of newline/tab delimited data,
-- padding columns to specified amount
readTable :: FilePath -> IO [[String]]
readTable filename = do f <- readFile filename
                        let lines = filter (/= "") $ splitRows f
                            arr = map (padCols . splitCols) lines
                        return arr
    where
      splitRows s = split s '\n'
      splitCols s = split s '\t'
      padCols = (++ (repeat ""))

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim

-- Utility functions that handle null data
-- and return appropriate defaults
readInt :: String -> Int
readInt "" = 0
readInt s = read s

makeItems :: String          -- Filename to parse
          -> ([String] -> a) -- function that takes a list of data and creates an item
          -> IO [a]
makeItems filename constructor = do
  rows <- readTable (Settings.old_data_path ++ filename)
  return $ map constructor rows

readCategories = makeItems "categories.txt" mkCat
    where mkCat row = C.Category { C.id = read (row !! 0),
                                   C.name = row !! 1}
writeItems cn writer items = mapM (writer cn) items

addCategory cn c =  DB.doInsert cn "categories"
                    ["id",
                     "name"]
                    [toSql $ C.id c,
                     toSql $ C.name c]
                    >> return c

makeSlug = id -- TODO

readPosts = makeItems "posts.txt" mkPost
            >>= mapM addFullText
            >>= return . sortBy (comparing P.timestamp)
    where mkPost row = P.Post { P.id = read (row !! 0),
                                P.title = row !! 1,
                                P.slug = "",
                                P.post_raw = "",
                                P.post_formatted = "",
                                P.summary_raw = row !! 4,
                                P.summary_formatted = row !! 4,
                                P.format_id = Formats.rawhtml,
                                P.timestamp = read (row !! 2),
                                P.comments_open = True
                              }
          addFullText p = do f <- readFile (Settings.old_data_path ++ "posts/" ++ (show $ P.id p))
                                  -- TODO: replace '&#10;' with '\n'
                                  -- TODO: encoding
                             return p { P.post_raw = f, P.post_formatted = f }

addPost cn p = do { slug <- makeSlug cn p;
                    let p = p { P.slug = slug } in
                    DB.doInsert cn "posts"
                    ["title",
                     "slug",
                     "post_raw",
                     "post_formatted",
                     "summary_raw",
                     "summary_formatted",
                     "format_id",
                     "timestamp",
                     "comments_open"]
                    [toSql $ P.title p,
                     toSql $ P.slug p,
                     toSql $ P.post_raw p,
                     toSql $ P.post_formatted p,
                     toSql $ P.summary_raw p,
                     toSql $ P.summary_formatted p,
                     toSql $ P.format_id p,
                     toSql $ P.timestamp p,
                     toSql $ P.comments_open p];
                    [[newid]] <- quickQuery cn "SELECT last_insert_rowid();" [];
                    return p { P.id = fromSql $ newid } ;
                  }

readPostCategories = makeItems "postcategories.txt" mkPostCategory
    where mkPostCategory row = (read (row !! 0),
                                read (row !! 1)) :: (Int, Int)

addPostCategory cn pc = do { DB.doInsert cn "post_categories"
                             ["post_id",
                              "category_id"]
                             [toSql $ fst pc,
                              toSql $ snd pc];
                             return pc; }

main = handleSqlError $ do
  cn <- DB.connect
  cats <- readCategories
  writeItems cn addCategory cats
  origPosts <- readPosts
  newPosts <- writeItems cn addPost origPosts
  -- we need the new IDs to rewrite comments tables
  -- and the post/categories m2m
  let id_map = Map.fromList $ zip (map P.id origPosts) (map P.id newPosts)
  postCategories' <- readPostCategories
  let postCategories = correctIds postCategories' id_map
  writeItems cn addPostCategory postCategories

  -- TODO: derive a blog.php script that contains the new mappings
  -- for posts and categories and will do redirects to the new URLs
  commit cn
  return ()

    where correctIds pcs id_map = map (\(p_id, c_id) -> (fromJust $ Map.lookup p_id id_map, c_id)) pcs

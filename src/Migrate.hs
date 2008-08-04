import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as UTF8
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
import GHC.Unicode (toLower)
import Monad (liftM)
import Utils (regexReplace)
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
                                   C.name = row !! 1,
                                   C.slug = ""}
writeItems cn writer items = mapM (writer cn) items

addCategory cn c =  do slug <- makeCategorySlug cn c
                       let c2 = c { C.slug = slug }
                       DB.doInsert cn "categories"
                             ["id",
                              "name",
                              "slug"]
                             [toSql $ C.id c2,
                              toSql $ C.name c2,
                              toSql $ C.slug c2]
                       return c2

slugFromTitle title = map toLower $ UTF8.toString $
                      regexReplace (B.pack "-+$") (B.pack "") $
                      regexReplace (B.pack "[^A-Za-z0-9]+") (B.pack "-") (B.pack title)

makePostSlug cn p = makeSlugGeneric cn (slugFromTitle $ P.title p) "posts" 1

makeCategorySlug cn cat = makeSlugGeneric cn (slugFromTitle $ C.name cat) "categories" 1

makeSlugGeneric cn slugBase table iter = do
  let slugAttempt =  (slugBase ++ makeSuffix iter);
  [[SqlString c]] <- quickQuery cn ("SELECT count(slug) FROM " ++ table ++ " WHERE slug = ?") [toSql slugAttempt];
  case c of
    "0" -> return slugAttempt
    _   -> makeSlugGeneric cn slugBase table (iter + 1)

 where
   makeSuffix 1 = ""
   makeSuffix n = show n

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
                             let fixed = B.unpack $ regexReplace (B.pack "&#10;") (B.pack "\n") (B.pack f)
                             return p { P.post_raw = fixed, P.post_formatted = fixed }

addPost cn p = do { slug <- makePostSlug cn p;
                    p2 <- return $ p { P.slug = slug };
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
                    [toSql $ P.title p2,
                     toSql $ P.slug p2,
                     toSql $ P.post_raw p2,
                     toSql $ P.post_formatted p2,
                     toSql $ P.summary_raw p2,
                     toSql $ P.summary_formatted p2,
                     toSql $ P.format_id p2,
                     toSql $ P.timestamp p2,
                     toSql $ P.comments_open p2];
                    [[newid]] <- quickQuery cn "SELECT last_insert_rowid();" [];
                    return p2 { P.id = fromSql $ newid } ;
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

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BL
import qualified Category as C
import qualified Post as P
import qualified Settings
import qualified DB
import qualified Formats
import Database.HDBC
import List (sortBy, intersperse)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import GHC.Unicode (toLower)
import Monad (liftM)
import Text.Template (readTemplate, renderToFile)
import Utils (regexReplace)
-- Migration script for the old data

-- Misc utilities
split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs delim

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
                             ["name",
                              "slug"]
                             [toSql $ C.name c2,
                              toSql $ C.slug c2]
                       [[newid]] <- quickQuery cn "SELECT last_insert_rowid();" [];
                       return c2 { C.id = fromSql $ newid }

slugFromTitle title = map toLower $ B.unpack $
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

utf8 = UTF8.fromString

makePHPMap amap = "array(" ++
                  (concat $ intersperse ",\n" $ map mkPair $ Map.toList amap)
                  ++ ")"
    where mkPair (a,b) = (show a) ++ " => " ++ (show b)

createRedirectFile postUrlMap categoryUrlMap = do
    tpl <- readTemplate Settings.redirect_file_template
    let ctx = Map.fromList ([(utf8 "postIdsToUrls", utf8 $ makePHPMap postUrlMap),
                             (utf8 "categoryIdsToUrls", utf8 $ makePHPMap categoryUrlMap)])
    renderToFile Settings.redirect_file_output tpl ctx

-- TODO - a better way of generating this, something like Routes
makePostUrl p = Settings.root_url ++ "posts/" ++ (P.slug p) ++ "/"
makeCategoryUrl c = Settings.root_url ++ "categories/" ++ (C.slug c) ++ "/"

main = handleSqlError $ do
  cn <- DB.connect
  origCats <- readCategories
  newCats <- writeItems cn addCategory origCats
  origPosts <- readPosts
  newPosts <- writeItems cn addPost origPosts
  -- we need the new/old IDs of posts/categories to rewrite comments tables
  -- and the post/categories m2m
  let post_id_map = Map.fromList $ zip (map P.id origPosts) (map P.id newPosts)
  let cat_id_map = Map.fromList $ zip (map C.id origCats) (map C.id newCats)
  postCategories' <- readPostCategories
  let postCategories = correctIds postCategories' post_id_map cat_id_map
  writeItems cn addPostCategory postCategories

  let postUrlMap = Map.fromList $ zip (map (show . P.id) origPosts) (map makePostUrl newPosts)
  let categoryUrlMap = Map.fromList $ zip (map (show . C.id) origCats) (map makeCategoryUrl newCats)
  createRedirectFile postUrlMap categoryUrlMap
  commit cn
  return ()

    where correctIds pcs p_id_map c_id_map =
              map (\(p_id, c_id) -> (fromJust $ Map.lookup p_id p_id_map,
                                     fromJust $ Map.lookup c_id c_id_map)) pcs

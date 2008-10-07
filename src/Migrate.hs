import Blog.Utils (regexReplace, split)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Database.HDBC
import List (sortBy, intersperse)
import Monad (liftM)
import Text.Template (readTemplate, renderToFile)
import qualified Blog.Category as C
import qualified Blog.DB as DB
import qualified Blog.Formats as Formats
import qualified Blog.Post as P
import qualified Blog.Links as Links
import qualified Blog.Settings as Settings
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Map as Map
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

makeItems :: String          -- Filename to parse
          -> ([String] -> a) -- function that takes a list of data and creates an item
          -> IO [a]
makeItems filename constructor = do
  rows <- readTable (Settings.old_data_path ++ filename)
  return $ map constructor rows

readCategories = makeItems "categories.txt" mkCat
    where mkCat row = C.Category { C.uid = read (row !! 0),
                                   C.name = row !! 1,
                                   C.slug = ""}

readPosts = makeItems "posts.txt" mkPost
            >>= mapM addFullText
            >>= return . sortBy (comparing P.timestamp)
    where mkPost row = P.Post { P.uid = read (row !! 0),
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
          addFullText p = do let dataFile = Settings.old_data_path ++ "posts/" ++ (show $ P.uid p)
                             f <- readFile dataFile
                             let fixed = fixCodes f
                             return p { P.post_raw = fixed,
                                        P.post_formatted = fixed }
          fixCodes txt = B.unpack $ regexReplace (B.pack "&#10;") (B.pack "\n") (B.pack txt)

readPostCategories = makeItems "postcategories.txt" mkPostCategory
    where mkPostCategory row = (read (row !! 0),
                                read (row !! 1)) :: (Int, Int)

writeItems cn writer items = mapM (writer cn) items

utf8 = UTF8.fromString

makePHPMap amap = "array(" ++
                  (concat $ intersperse ",\n" $ map arrayPair $ Map.toList amap)
                  ++ ")"
    where arrayPair (a,b) = (show a) ++ " => " ++ (show b) -- doesn't handle
                                                           -- funny chars, but
                                                           -- it works for now

createRedirectFile postUrlMap categoryUrlMap = do
    tpl <- readTemplate Settings.redirect_file_template
    let ctx = Map.fromList ([(utf8 "postIdsToUrls",
                              utf8 $ makePHPMap postUrlMap),
                             (utf8 "categoryIdsToUrls",
                              utf8 $ makePHPMap categoryUrlMap)])
    renderToFile Settings.redirect_file_output tpl ctx

main = handleSqlError $ do
  cn <- DB.connect
  origCats <- readCategories
  newCats <- writeItems cn C.addCategory origCats
  origPosts <- readPosts
  newPosts <- writeItems cn P.addPost origPosts
  -- we need the new/old IDs of posts/categories to rewrite comments tables
  -- and the post/categories m2m
  let post_id_map = Map.fromList $ zip (map P.uid origPosts) (map P.uid newPosts)
  let cat_id_map = Map.fromList $ zip (map C.uid origCats) (map C.uid newCats)
  postCategories' <- readPostCategories
  let postCategories = correctIds postCategories' post_id_map cat_id_map
  writeItems cn P.addPostCategory postCategories

  let postUrlMap = Map.fromList $ zip (map (show . P.uid) origPosts)
                                      (map Links.postLink newPosts)
  let categoryUrlMap = Map.fromList $ zip (map (show . C.uid) origCats)
                                          (map Links.categoryLink newCats)
  createRedirectFile postUrlMap categoryUrlMap
  commit cn
  return ()

    where correctIds pcs p_id_map c_id_map =
              map (\(p_id, c_id) -> (fromJust $ Map.lookup p_id p_id_map,
                                     fromJust $ Map.lookup c_id c_id_map)) pcs

import Blog.Model
import Blog.Utils (regexReplace, split)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Database.HDBC
import List (sortBy, intersperse)
import Monad (liftM)
import Text.Template (readTemplate, renderToFile)
import qualified Blog.Category as C
import qualified Blog.Comment as Cm
import qualified Blog.DB as DB
import qualified Blog.Formats as Formats
import qualified Blog.Post as P
import qualified Blog.Links as Links
import qualified Blog.Settings as Settings
import qualified Data.ByteString.Lazy.Char8 as LB
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


-- Reading functions
readCategories = makeItems "categories.txt" mkCat
    where mkCat row = C.Category { C.uid = read (row !! 0)
                                 , C.name = row !! 1
                                 , C.slug = ""
                                 }

readPosts = makeItems "posts.txt" mkPost
            >>= mapM addFullText
            >>= mapM (return . fixEmptyFullTexts)
            >>= return . sortBy (comparing P.timestamp)
    where mkPost row = P.Post { P.uid = read (row !! 0)
                              , P.title = row !! 1
                              , P.slug = ""
                              , P.post_raw = ""
                              , P.post_formatted = ""
                              , P.summary_raw = row !! 4
                              , P.summary_formatted = row !! 4
                              , P.format = Formats.Rawhtml
                              , P.timestamp = read (row !! 2)
                              , P.comments_open = True
                              }
          addFullText p = do let dataFile = Settings.old_data_path ++ "posts/" ++ (show $ P.uid p)
                             f <- readFile dataFile
                             let fixed = fixCodes f
                             return p { P.post_raw = fixed,
                                        P.post_formatted = fixed }
          fixEmptyFullTexts p = if null $ P.post_raw p
                                then p { P.post_raw = P.summary_raw p
                                       , P.post_formatted = P.summary_formatted p
                                       }
                                else p
          -- Fix dodgy stuff, and reinterpret as UTF8
          fixCodes txt = UTF8.toString $ regexReplace (LB.pack "&#10;") (LB.pack "\n") (LB.pack txt)

readPostCategories = makeItems "postcategories.txt" mkPostCategory
    where mkPostCategory row = (read (row !! 0),
                                read (row !! 1)) :: (Int, Int)

readComments = makeItems "comments.txt" mkComment
               >>= return . sortBy (comparing Cm.timestamp)
    where mkComment row = Cm.Comment { Cm.uid = read (row !! 0)
                                     , Cm.post_id = read (row !! 1)
                                     , Cm.timestamp = read (row !! 2)
                                     , Cm.name = row !! 3
                                     , Cm.email = row !! 4
                                     , Cm.text_raw = row !! 5
                                     , Cm.text_formatted = row !! 5
                                     , Cm.format = Formats.Rawhtml
                                     , Cm.hidden = False
                                     , Cm.response = ""
                                     }
-- Writing

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
    let ctx = Map.fromList [(utf8 "postIdsToUrls", utf8 $ makePHPMap postUrlMap)
                           ,(utf8 "categoryIdsToUrls", utf8 $ makePHPMap categoryUrlMap)]
    renderToFile Settings.redirect_file_output tpl ctx

-- Misc fixes
-- Titles of all article have HTML in them, which is difficult to fix
-- up.  They are only announcements, so we just delete.
articlePosts = "select posts.id FROM posts INNER JOIN post_categories ON posts.id = post_categories.post_id INNER JOIN categories ON post_categories.category_id = categories.id WHERE categories.slug = 'articles';"
deletePost = "DELETE FROM posts WHERE id = ?;";
deletePC   = "DELETE FROM post_categories WHERE post_id = ?;"
deleteArticlePosts cn = do
  ids' <- quickQuery cn articlePosts []
  let ids = map (fromSql . head) ids' :: [Int]
  mapM_ (\x -> quickQuery cn deletePost [toSql $ x]) ids
  mapM_ (\x -> quickQuery cn deletePC [toSql $ x]) ids


-- Main
main = handleSqlError $ do
  cn <- DB.connect
  -- Categories
  origCats <- readCategories
  newCats <- writeItems cn addCategory origCats
  -- Posts
  origPosts <- readPosts
  newPosts <- writeItems cn addPost origPosts
  -- we need the new/old IDs of posts/categories to rewrite comments tables
  -- and the post/categories m2m
  let post_id_map = Map.fromList $ zip (map P.uid origPosts) (map P.uid newPosts)
  let cat_id_map = Map.fromList $ zip (map C.uid origCats) (map C.uid newCats)

  -- post-categories
  postCategories' <- readPostCategories
  let postCategories = correctIds postCategories' post_id_map cat_id_map
  writeItems cn addPostCategory postCategories

  -- comments
  comments' <- readComments
  let comments = correctCommentPostIds comments' post_id_map
  writeItems cn addComment comments

  -- misc fixes
  deleteArticlePosts cn

  -- Redirect file
  let postUrlMap = Map.fromList $ zip (map (show . P.uid) origPosts)
                                      (map Links.postUrl newPosts)
  let categoryUrlMap = Map.fromList $ zip (map (show . C.uid) origCats)
                                          (map Links.categoryUrl newCats)
  createRedirectFile postUrlMap categoryUrlMap

  createUser cn "luke" True
  setPassword cn "luke" "test"

  commit cn
  return ()

    where correctIds pcs p_id_map c_id_map =
              map (\(p_id, c_id) -> (fromJust $ Map.lookup p_id p_id_map,
                                     fromJust $ Map.lookup c_id c_id_map)) pcs
          correctCommentPostIds cms p_id_map =
              map (\cm -> cm { Cm.post_id = fromJust $ Map.lookup (Cm.post_id cm) p_id_map }) cms

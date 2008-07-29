import qualified Category as C
import qualified Post as P
import qualified Settings
import qualified DB
import qualified Formats
import Database.HDBC
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
{-
writeItems writer items = mapM 
                           (\i -> do cn <- DB.connect
                                     newitem <- writer cn i
                                     commit cn
                                     disconnect cn
                                     return newitem) 
                           items
-}
writeItems writer items = do cn <- DB.connect
                             newitems <- mapM (writer cn) items
                             commit cn
                             disconnect cn
                             return newitems

addCategory cn c =  DB.doInsert cn "categories" 
                    ["id", 
                     "name"] 
                    [toSql $ C.id c, 
                     toSql $ C.name c]
                    >> return c

makeSlug = id -- TODO

readPosts = makeItems "posts.txt" mkPost >>= mapM addFullText
    where mkPost row = P.Post { P.id = read (row !! 0),
                                P.title = row !! 1,
                                P.slug = makeSlug (row !! 1),
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

addPost cn p = do { DB.doInsert cn "posts" 
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
                    newid <- quickQuery cn "SELECT max(id) FROM posts;" [];
                    return p { P.id = fromSql $ head $ head newid } ; }

main = handleSqlError $ do          
  cats <- readCategories >>= (writeItems addCategory)
  origPosts <- readPosts
  newPosts <- writeItems addPost origPosts
  return ()
